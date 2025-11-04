
zonal <- function(v, region) {
  print(glue::glue("working on {v} or {region}..."))
  rasts <- terra::rast(glue::glue("~/data/gridmet/montana/{v}/aggregated/") %>%
                         list.files(full.names = T, pattern = "monthly"))
  shp = sf::read_sf(glue::glue("https://mco-normals.s3.us-east-2.amazonaws.com/fgb/{region}.fgb"))

  names(rasts) <- terra::time(rasts)

  shp <- sf::st_transform(shp, crs = sf::st_crs(rasts))
  shp_as_rast <- shp %>%
    terra::vect() %>%
    terra::rasterize(rasts, field=attr_id)

  out <- terra::zonal(rasts, shp_as_rast, fun=fun, na.rm = T) %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(-id, names_to = "date")

  readr::write_csv(out, glue::glue("./data/zonal/{region}_{v}.csv"))

}

regions = c("blm", "counties", "hucs", "tribes")
variables = c("erc", "etr", "pet", "pr", "rmax", "rmin", "sph", "srad", "th", "tmmn", "tmmx", "vpd", "vs")

tidyr::crossing(regions, variables) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(out = list(zonal(variables, regions)))


smart_read <- function(x) {
  dat <- readr::read_csv(x, show_col_types = FALSE)
  if (!("variable" %in% names(dat))) {
    dat %<>%
      dplyr::mutate(
        variable = basename(x) %>%
          tools::file_path_sans_ext() %>%
          stringr::str_split_1("_") %>%
          tail(1)
      )
  }

  if ("name" %in% names(dat)) {
    dat %<>%
      dplyr::select(-name)
  }

  return(dat)
}

library(magrittr)
dat <- list.files("./data/zonal_stats", pattern = ".csv", full.names = T) %>%
  purrr::map(smart_read) %>%
  dplyr::bind_rows() %>%
  tidyr::separate(id, c("type", "id", "name"), sep = "_") %>%
  dplyr::mutate(variable = ifelse(variable == "mean", "gpp", variable))

dat %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(variable = dplyr::case_when(
    variable == "ET" ~ "m16_et",
    variable == "PET" ~ "m16_pet",
    .default = tolower(variable)
  )) %>%
  arrow::write_dataset(
    "./data/zonal",
    format = "parquet",
    partitioning = c("type", "id","variable")
  )
