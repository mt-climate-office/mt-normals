#' write_as_cog
#'
#' @description A wrapper around `terra::writeRaster` to write a `terra::rast`
#' to disk as a [Cloud Optimized GeoTIFF](https://www.cogeo.org/).
#'
#' @param x
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
write_as_cog <- function(x, filename) {
  terra::writeRaster(
    x = x,
    filename = filename,
    overwrite = TRUE,
    gdal = c("COMPRESS=DEFLATE",
             "of=COG"),
    memfrac = 0.9
  )
}


#' read_from_server
#'
#' @importFrom magrittr %$%
#' @description Read a cog from the MCO file server and also read in the associated metadata.
#' Without using this function, the raster will not have a `terra::time` attribute.
#'
#' @param f_url The string of the url path to the cog on the file server.
#'
#' @return
#' @export
#'
#' @examples
#' read_from_server("https://data.climate.umt.edu/mca/cmip/ACCESS-ESM1-5_historical_r1i1p1f1_above90.tif")
read_from_server <- function(f_url) {

  r <- terra::rast(f_url)

  # For some reason the date of Jan 1st gets messed up in the metadata so this
  # fixes it.
  dates <- paste0(f_url, ".aux.json") %>%
    purrr::map(jsonlite::read_json) %>%
    purrr::map(function(x) x$time) %>%
    unlist() %>%
    tibble::tibble(d = .) %>%
    tidyr::separate(d, c("year", "month", "day")) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.numeric(.x))) %>%
    dplyr::mutate(
      year = ifelse(month == 13, year+1, year),
      month = ifelse(month == 13, 1, month),
      out = glue::glue("{year}-{month}-{day}") %>%
        lubridate::as_date()
    ) %$%
    out

  terra::time(r) <- dates
  r
}


deg_c_to_f <- function(t) {
  (t * 1.8) + 32
}


subset_to_reference <- function(r, reference_period) {
  tibble::tibble(
    time = terra::time(r)
  ) %>%
    dplyr::mutate(
      year = lubridate::year(time),
      idx = 1:dplyr::n()
    ) %>%
    dplyr::filter(
      year %in% reference_period[1]:reference_period[2]
    ) %$%
    terra::subset(r, idx)
}

assign_time_and_name <- function(r, annual, descriptor) {

  date_fmt <- ifelse(annual, "0101", "01")
  terra::time(r) <- names(r) %>%
    paste0(date_fmt) %>%
    lubridate::as_date(format = "X%Y%m%d")

  names(r) <- paste(descriptor, 1:terra::nlyr(r), sep = "_")
  return(r)
}

calc_days_above_90 <- function(r, is.kelvin=TRUE) {

  # r <- subset_to_reference(r, reference_period)

  if (is.kelvin) r <- deg_c_to_f(r - 273.15)
  r[r < 90] = 0
  r[r >= 90] = 1

  terra::tapp(r, index="yearmonths", fun="sum") %>%
    assign_time_and_name(FALSE, "abv90")
}

calc_freeze_free_days <- function(r, is.kelvin=TRUE) {

  if (is.kelvin) r <- r - 273.15
  r[r <= 0] = 0
  r[r > 0] = 1

  terra::tapp(r, index="yearmonths", fun="sum") %>%
    assign_time_and_name(FALSE, "freeze-free")
}


calc_wet_days <- function(r, is.base.units=TRUE) {

  if (is.base.units) {
    r <- (r * 86400)/24
  }

  r[r < 1] <- 0
  r[r >= 1] <- 1

  r
}

calc_dry_days <- function(r, is.base.units=TRUE) {

  if (is.base.units) {
    r <- (r * 86400)/24
  }

  m <- c(0, 0.01, 1,
         0.01, Inf, 0)
  m <- matrix(m, ncol=3, byrow=TRUE)

  terra::classify(r, m, include.lowest=TRUE)
}

get_consecutive_days <- function(r, count_value = 1, time_index="years") {

  out <- terra::tapp(
    r, index=time_index,
    fun = function(x) {
      vals <- rle(x)
      max(vals$lengths[which(vals$values == count_value)])
    }
  )
  out[out == -Inf] <- 0
  out

}

# Formulas from https://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml
calc_heat_index <- function(temp, rh, is.fahrenheit=FALSE) {
  if (!is.fahrenheit) {
    temp <- temp - 273.15
    temp <- deg_c_to_f(temp)
  }

  if (temp <= 40) {
    return(temp)
  }

  hi <- 0.5 * (temp + (61.0 + ((temp-68)*1.2) + (rh*0.094)))

  if (hi < 80) {
    return(hi)
  }

  hi <- -42.379 + 2.04901523 * temp +
    10.14333127 * rh - 0.22475541 * temp * rh - 0.00683783 * temp * temp -
    0.05481717 * rh * rh + 0.00122874 * temp * temp * rh + 0.00085282 * temp * rh * rh -
    0.00000199 * temp * temp * rh * rh

  if ((rh < 13) & (temp > 80) & (temp < 112)) {
    adj <- ((13-rh)/4)*sqrt((17-abs(temp-95))/17)
    return(hi - adj)
  }

  if ((rh > 85) & (temp > 80) & (temp < 87)) {
    adj <- ((rh-85)/10) * ((87-temp)/5)

    return(hi + adj)
  }

  return(hi)
}

calc_heat_index <- Vectorize(calc_heat_index)
spatial_heat_index <- function(temp, rh, is.fahrenheit) {
  # Still need to debug this!!!!
  sds <- terra::sds(temp, rh)
  terra::lapp(sds, calc_heat_index, is.fahrenheit=FALSE, cores=10) -> a
}

save_daily_heat_index <- function(data_dir) {

  test <- list.files(data_dir, full.names = T, pattern = ".tif", recursive = F) %>%
    grep(".json", ., value = T, invert = T) %>%
    grep("tasmax.tif|hurs.tif", ., value = T) %>%
    tibble::tibble(f = .) %>%
    dplyr::mutate(base = basename(f)) %>%
    tidyr::separate(
      base, c("model", "scenario", "run", "variable"), sep="_"
    ) %>%
    head(4) %>%
    dplyr::mutate(
      r = list(terra::rast(f)),
      variable = tools::file_path_sans_ext(variable)
    ) %>%
    dplyr::select(-f) %>%
    tidyr::pivot_wider(names_from = variable, values_from = r) %>%
    dplyr::rowwise()
    dplyr::summarize(heat_index = list(calc_heat_index(temp = tasmax, rh = hurs, is.fahrenheit = FALSE)))
}

data_dir = "~/MCO_onedrive/General/nexgddp_cmip6_montana/data-derived/nexgddp_cmip6"
out_dir = "~/MCO_onedrive/General/nexgddp_cmip6_montana/data-derived/nexgddp_cmip6/monthly/derived"

calc_derived_metrics <- function(data_dir, out_dir) {
  tmmx <- list.files(data_dir, full.names = T, pattern = "tasmax.tif") %>%
    grep(".json", ., value = T, invert = TRUE)
  tmmn <- list.files(data_dir, full.names = T, pattern = "tasmin.tif") %>%
    grep(".json", ., value = T, invert = TRUE)
  pr <- list.files(data_dir, full.names = T, pattern = "pr.tif") %>%
    grep(".json", ., value = T, invert = TRUE)

  purrr::map(tmmx, function(x) {
    print(x)
    r <- terra::rast(x)
    above_90 <- calc_days_above_90(r, TRUE)
    out_name <- basename(stringr::str_replace(x, "tasmax.tif", "above90.tif"))
    out_path <- file.path(out_dir, out_name)
    write_as_cog(above_90, out_path)
  })

  purrr::map(tmmn, function(x) {
    print(x)
    r <- terra::rast(x)
    freeze_free <- calc_freeze_free_days(r, TRUE)
    out_name <- basename(stringr::str_replace(x, "tasmin.tif", "freeze-free.tif"))
    out_path <- file.path(out_dir, out_name)
    write_as_cog(freeze_free, out_path)
  })

  purrr::map(pr, function(x) {
    print(x)
    r <- terra::rast(x)
    dry <- calc_dry_days(r, TRUE)
    wet <- calc_wet_days(r, TRUE)

    get_consecutive_days(dry) %>%
      assign_time_and_name(TRUE, "con-dry") %>%
      write_as_cog(
        file.path(
          out_dir,
          basename(stringr::str_replace(x, "pr.tif", "con-dry.tif"))
        )
      )

    con_wet <- get_consecutive_days(wet) %>%
      assign_time_and_name(TRUE, "con-wet") %>%
      write_as_cog(
        file.path(
          out_dir,
          basename(stringr::str_replace(x, "pr.tif", "con-wet.tif"))
        )
      )

    terra::tapp(dry, "yearmonths", "sum") %>%
      assign_time_and_name(FALSE, "dry-days") %>%
      write_as_cog(
        file.path(
          out_dir,
          basename(stringr::str_replace(x, "pr.tif", "dry-days.tif"))
        )
      )

    terra::tapp(wet, "yearmonths", "sum") %>%
      assign_time_and_name(FALSE, "wet-days") %>%
      write_as_cog(
        file.path(
          out_dir,
          basename(stringr::str_replace(x, "pr.tif", "wet-days.tif"))
        )
      )
  })
}

#' spat_summary
#'
#' @param rasts A `terra::rast` that zonal statistics will be derived from.
#' @param shp An `sf` object that will be used to derive zonal statistics.
#' @param attr_id A string of the column name in `shp` describing each subregion.
#' @param name_to The desired name of the descriptor column in the output dataframe.
#' @param fun The function to apply in the zonal statistics.
#' @param ... Any additional arguments that should be passed to fun
#'
#' @return A dataframe of the aggregated statistics for each region in `shp`.
#' @export
#'
#' @examples
spat_summary <- function(rasts, shp, attr_id = NULL, name_to = "timescale", fun, ...) {

  if (is.null(attr_id)) {
    shp %<>%
      dplyr::mutate(id = 1:dplyr::n())

    attr_id <- "id"
  }

  checkmate::assert_true(
    length(unique(names(rasts))) == terra::nlyr(rasts),
  )

  shp <- sf::st_transform(shp, crs = sf::st_crs(rasts))
  shp_as_rast <- shp %>%
    terra::vect() %>%
    terra::rasterize(rasts, field=attr_id)

  terra::zonal(rasts, shp_as_rast, fun=fun, na.rm = T, ...) %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(-!!rlang::sym(attr_id), names_to = name_to) %>%
    dplyr::full_join(shp, by = attr_id)
}
