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
    tidyr::pivot_longer(-id, names_to = "date")

  # readr::write_csv(out, glue::glue("~/data/cmip_tables/{model}_{region}_{v}_{scenario}.csv"))
  return(out)
}


regions = tibble::tibble(region=c("blm", "counties", "hucs", "tribes")) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(shp = glue::glue("https://mco-normals.s3.us-east-2.amazonaws.com/fgb/{region}.fgb") %>%
                  sf::read_sf() %>% list())

joined %>%
  tidyr::expand_grid(regions) %>%
  dplyr::rowwise() %>%
  dplyr::filter(variable == 'huss', scenario == 'ssp126') %>%
  dplyr::mutate(summary = list(zonal(variable, r, region, scenario, model, shp)))


future::plan(future::multisession, workers = 5)

list.files("~/data/cmip_tables/", full.names = T, pattern = '.csv') %>%
  tibble::tibble(f = .) %>%
  dplyr::mutate(meta = basename(f) %>%
                  tools::file_path_sans_ext()) %>%
  tidyr::separate(meta, c("model", "type", "variable", "scenario"), sep = "_") %>%
  dplyr::filter(variable == "eto") %>%
  dplyr::group_by(type, scenario, variable) %>%
  dplyr::group_split() %>%
  purrr::map(function(x) {
    tmp <- x %>%
      dplyr::rowwise() %>%
      dplyr::mutate(dat = list(readr::read_csv(f, show_col_types = F))) %>%
      tidyr::unnest(dat) %>%
      dplyr::select(-f)

    if (is.numeric(tmp$date[[1]])) {
      tmp <- tmp %>%
        dplyr::mutate(
          date = date %>%
            zoo::as.yearmon() %>%
            zoo::as.Date(frac = 0)
        )
    }

    tmp %>%
      dplyr::group_by(variable, id, year=lubridate::year(date), scenario, model) %>%
      dplyr::summarise(
        value = ifelse(unique(variable) %in% c("pr", "eto", "hargreaves", "above90", "con-dry", "con-wet",
                                "dry-days", "freeze-free", "gdd", "wet-days", "et_m16", "pet_m16",
                                "gpp", "afgnpp", "pfgnpp", "shrnpp", "trenpp"), sum(value), mean(value)),
        .groups = "drop"
      ) %>%
      dplyr::group_by(year, id, variable, scenario) %>%
      dplyr::summarise(
        val = mean(value) %>% round(3),
        lower = quantile(value, 0.1) %>% round(3),
        upper = quantile(value, 0.9) %>% round(3),
        .groups = "drop"
      ) %>%
      dplyr::mutate(plot_type = "timeseries") %>%
      tidyr::separate(id, c("type", "id", "name"), sep = "_") %>%
      dplyr::group_by(type, id, variable, scenario) %>%
      dplyr::mutate(baseline = mean(val[year >= 1995 & year <= 2024], na.rm = TRUE)) %>%
      arrow::write_dataset(
        "~/data/cmip_zonal",
        format = "parquet",
        partitioning = c("plot_type", "type", "id","variable")
      )

    tmp %>%
      dplyr::mutate(
        year = lubridate::year(date),
        month = month.abb[lubridate::month(date)],
        grp = dplyr::case_when(
          year %in% 1995:2024 ~ "Reference Period (1995-2024)",
          year %in% 2040:2069 ~ "Mid Century (2040-2069)",
          year %in% 2970:2099 ~ "End-of-Century (2070-2099)"
        ),
        scenario = ifelse(year >= 2015 & year <= 2024, "historical", scenario)
      )  %>%
      dplyr::filter(!is.na(grp)) %>%
      dplyr::group_by(scenario, month, grp, variable, id) %>%
      dplyr::summarise(
        upper = quantile(value, 0.9) %>% as.numeric() %>% round(3),
        lower = quantile(value, 0.1) %>% as.numeric() %>% round(3),
        value = median(value) %>% round(3),
        .groups = "drop"
      ) %>%
      dplyr::mutate(plot_type = "monthly") %>%
      tidyr::separate(id, c("type", "id", "name"), sep = "_") %>%
      arrow::write_dataset(
        "~/data/cmip_zonal",
        format = "parquet",
        partitioning = c("plot_type", "type", "id","variable")
      )


  }, .progress = TRUE)



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
