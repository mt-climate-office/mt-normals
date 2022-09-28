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

deg_c_to_f <- function(t) {
  (t * 1.8) + 32
}

# Formulas from https://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml
calc_heat_index <- function(temp, rh, is.fahrenheit=FALSE) {
  if (!is.fahrenheit) {
    temp <- deg_c_to_f(temp)
  }

  if (t <= 40) {
    return(t)
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

calc_days_above_90 <- function(r, is.kelvin=TRUE, reference_period=c(1981, 2010)) {

  r <- subset_to_reference(r, reference_period)

  if (is.kelvin) r <- deg_c_to_f(r - 273.15)
  r[r < 90] = 0
  r[r >= 90] = 1

  terra::tapp(r, index="years", fun="sum") %>%
    terra::app(fun = "mean")
}


calc_freeze_free_days <- function(r, is.kelvin=TRUE, reference_period=c(1981, 2010)) {

  r <- subset_to_reference(r, reference_period)
  if (is.kelvin) r <- r - 273.15
  r[r <= 0] = 0
  r[r > 0] = 1

  terra::tapp(r, index="years", fun="sum") %>%
    terra::app(fun = "mean")
}


calc_wet_days <- function(r, is.base.units=TRUE, reference_period=c(1981, 2010)) {

  r <- subset_to_reference(r, reference_period)
  if (is.base.units) {
    r <- (r * 86400)/24
  }

  r[r < 1] <- 0
  r[r >= 1] <- 1

  r
}

calc_dry_days <- function(r, is.base.units=TRUE, reference_period=c(1981, 2010)) {

  r <- subset_to_reference(r, reference_period)
  if (is.base.units) {
    r <- (r * 86400)/24
  }

  m <- c(0, 0.01, 1,
         0.01, Inf, 0)
  m <- matrix(m, ncol=3, byrow=TRUE)

  terra::classify(r, m, include.lowest=TRUE)
}

get_consecutive_days <- function(r, count_value = 1, time_index="years") {

  terra::tapp(
    r, index=time_index,
    fun = function(x) {
      vals <- rle(x)
      max(vals$lengths[which(vals$values == count_value)])
    }
  )

}
