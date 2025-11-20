library(magrittr)

dat <- list.files("~/data/cmip", full.names = T, pattern = ".tif") %>%
  grep(".json", ., value = T, invert = T) %>%
  tibble::tibble(f = .) %>%
  dplyr::mutate(bn = basename(f) %>%
                  tools::file_path_sans_ext()) %>%
  tidyr::separate(bn, c("model", "scenario", "drop", "variable"), sep = "_") %>%
  dplyr::select(-drop) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(r = list(terra::rast(f)))

historical <- dat %>%
  dplyr::filter(scenario == "historical")

append_historical <- function(m, v, r, historical) {

  tmp <- historical %>%
    dplyr::filter(model == !!m,
                  variable == !!v)

  c(tmp$r[[1]], r)
}

joined <- dat %>%
  dplyr::filter(scenario != "historical") %>%
  dplyr::group_by(model, scenario, variable) %>%
  dplyr::mutate(r =
    append_historical(
      dplyr::cur_group()$model,
      dplyr::cur_group()$variable,
      r[[1]],
      historical
    ) %>%
      list()
  )

ensemble_mean <- joined %>%
  dplyr::group_by(scenario, variable) %>%
  dplyr::summarise(
    r = list(terra::rast(r) %>% terra::tapp(index = "yearmonths", fun = "mean"))
  )

zonal <- function(v, rasts, region, scenario) {
  print(glue::glue("working on {v} for {region} for {scenario}..."))
  shp = sf::read_sf(glue::glue("https://mco-normals.s3.us-east-2.amazonaws.com/fgb/{region}.fgb"))

  names(rasts) <- terra::time(rasts)

  shp <- sf::st_transform(shp, crs = sf::st_crs(rasts))
  shp_as_rast <- shp %>%
    terra::vect() %>%
    terra::rasterize(rasts, field="id")

  out <- terra::zonal(rasts, shp_as_rast, fun="mean", na.rm = T) %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(-id, names_to = "date")

  readr::write_csv(out, glue::glue("~/data/cmip_tables/{region}_{v}_{scenario}.csv"))

}

regions = tibble::tibble(region=c("blm", "counties", "hucs", "tribes"))

ensemble_mean %>%
  tidyr::expand_grid(regions) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(summary = list(zonal(variable, r, region, scenario)))


out <- ensemble_mean %>%
  dplyr::mutate(
    fun = ifelse(variable %in% c("pr", "hargreaves", "dry-days", "freeze-free", "wet-days"), "sum", "mean")
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

smart_read <- function(x) {
  meta <- basename(x) %>%
    tools::file_path_sans_ext() %>%
    stringr::str_split_1("_") %>%
    magrittr::set_names(c("drop1", "type", "id", "plot_type", "drop2"))

  end <- lubridate::today() %>%
    lubridate::year() - 1

  start = end -29

  dat <- readr::read_csv(x, show_col_types=FALSE) %>%
    tibble::add_column(!!!as.list(meta)) %>%
    dplyr::select(-dplyr::starts_with("drop"))

  if (meta$plot_type == "monthly") {
    dat %>%
      dplyr::group_by(month)
  } else {
    avg <- dplyr::filter(dat, year >= start, year <= end) %>%
      dplyr::pull(value) %>%
      mean()

    dat %>%
      dplyr::mutate(avg = avg) %>%
      return()
  }


}

r <- terra::rast("~/data/hurs_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_1969_v1.1.nc")
bounds <- sf::read_sf("https://mco-normals.s3.us-east-2.amazonaws.com/fgb/hucs.fgb") %>%
  sf::st_bbox()


