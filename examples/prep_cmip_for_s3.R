library(magrittr)

dat <- list.files("~/data/cmip/", full.names = T, pattern = ".tif", recursive = T) %>%
  grep("monthly", ., value  = T) %>%
  grep(".json", ., value = T, invert = T) %>%
  tibble::tibble(f = .) %>%
  dplyr::mutate(bn = basename(f) %>%
                  tools::file_path_sans_ext()) %>%
  tidyr::separate(bn, c("variable", "model", "scenario"), sep = "_")

historical <- dat %>%
  dplyr::filter(scenario == "historical")

append_historical <- function(m, v, r, historical) {

  print(glue::glue("working on {m} for {v}..."))
  tmp <- historical %>%
    dplyr::filter(model == !!m,
                  variable == !!v)

  r1 = terra::rast(tmp$f[[1]])
  r2 = terra::rast(r)

  c(r1, r2)
}

joined <- dat %>%
  dplyr::filter(scenario != "historical") %>%
  dplyr::group_by(model, scenario, variable) %>%
  dplyr::filter(!(model == "MIROC6" && variable == "huss")) %>%
  dplyr::mutate(r =
    append_historical(
      dplyr::cur_group()$model,
      dplyr::cur_group()$variable,
      f,
      historical
    ) %>%
      list()
  )

ensemble_mean <- joined %>%
  dplyr::group_by(scenario, variable) %>%
  dplyr::summarise(
    r = list(terra::rast(r) %>% terra::tapp(index = "yearmonths", fun = "mean")),
    q1 = list(terra::rast(r) %>% terra::tapp(index = "yearmonths", fun = \(x) {quantile(x, 0.1)})),
    q9 = list(terra::rast(r) %>% terra::tapp(index = "yearmonths", fun = \(x) {quantile(x, 0.9)}))
  )

zonal <- function(v, rasts, region, scenario, model, shp) {
  print(glue::glue("working on {v} for {region} for {scenario} {model}..."))
  rasts = terra::rotate(rasts)
  names(rasts) <- terra::time(rasts)

  shp <- sf::st_transform(shp, crs = sf::st_crs(rasts))
  shp_as_rast <- shp %>%
    terra::vect() %>%
    terra::rasterize(rasts[[1]], field = 'id')

  out <- terra::zonal(rasts, shp_as_rast, fun="mean", na.rm = T) %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(-id, names_to = "date") %>%
    dplyr::mutate(date = lubridate::as_date(date))

  readr::write_csv(out, glue::glue("~/data/cmip_tables/{model}_{region}_{v}_{scenario}.csv"))

}

regions = tibble::tibble(region=c("blm", "counties", "hucs", "tribes")) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(shp = glue::glue("https://mco-normals.s3.us-east-2.amazonaws.com/fgb/{region}.fgb") %>%
                  sf::read_sf() %>% list())

joined %>%
  tidyr::expand_grid(regions) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(summary = list(zonal(variable, r, region, scenario, model, shp)))


out <- ensemble_mean %>%
  dplyr::mutate(
    fun = ifelse(variable %in% c("pr", "eto", "dry-days", "freeze-free", "wet-days", "con-dry", "con-wet"), "sum", "mean")
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    monthly = list(
      terra::tapp(r, index = "months", fun = "mean") %>%
        normals::write_as_cog(glue::glue("~/data/cmip_processed/{variable}_{scenario}_monthly.tif"))
    ),
    annual = list(
      terra::tapp(r, index = "years", fun = fun) %>%
        normals::write_as_cog(glue::glue("~/data/cmip_processed/{variable}_{scenario}_annual.tif"))
    )
  )

# TODO: Create rasters for each variable for baseline, mid century and end of century
# TODO: Create difference rasters relative to baseline
# TODO: Process the .csvs into the format needed for the web app.
