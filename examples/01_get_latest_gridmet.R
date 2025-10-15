library(normals)
library(magrittr)

bbox <- sf::read_sf("https://mco-normals.s3.us-east-2.amazonaws.com/fgb/hucs.fgb") %>%
  sf::st_bbox()

process_to_daily <- function() {

}


find_files_to_upload <- function(bucket) {

}

fetch_and_crop <- function() {

}

upload_to_s3 <- function(bucket, f) {

}


crop_daily_ncs <- function() {

  data_dir = "~/data/gridmet/"
  list.files("~/data/gridmet/raw", full.names = T, recursive = TRUE, pattern = ".nc") %>%
    grep("normals", ., invert=T, value = T) %>%
    purrr::map(\(x) {
      print(glue::glue("processing {x}..."))
      r <- terra::rast(x)
      dates <- terra::depth(r) %>%
        as.Date(origin = "1900-01-01")
      terra::time(r) <- dates

      varname <-  basename(dirname(x))
      out_dir <- file.path(data_dir, "montana", varname)

      if (!dir.exists(out_dir)) {
        dir.create(out_dir)
      }

      out_names <- glue::glue(
        "{out_dir}/{stringr::str_replace_all(dates, '-', '')}_{varname}.tif"
      )

      r %>%
        terra::crop(bbox) %>%
        terra::writeRaster(
          out_names,
          overwrite = TRUE,
          gdal = c("COMPRESS=DEFLATE",
                   "of=COG"),
          memfrac = 0.9
        )

      return(x)
    })
}


calc_normal_from_daily <- function(r) {
  normal_year = lubridate::today() %>%
    lubridate::year() %>%
    magrittr::subtract(1)
  file.path(data_dir, "montana") %>%
    list.files(full.names = T, include.dirs = T) %>%
    purrr::map(\(x) {
      variable = basename(x)
      func = ifelse(variable %in% c("pr", "etr", "pet"), "sum", "mean")
      out_dir <- file.path(x, "aggregated")
      if (!dir.exists(out_dir)) {
        dir.create(out_dir)
      }
      r <- list.files(x, full.names = T) %>%
        terra::rast()

      monthly <- normals::aggregate_daily(
        r = r,
        variable = variable,
        monthly = TRUE,
        agg_func = func,
      )
    })
}
