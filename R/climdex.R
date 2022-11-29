assign_time_and_name <- function(r, annual, descriptor) {

  date_fmt <- ifelse(annual, "0101", "01")
  terra::time(r) <- names(r) %>%
    paste0(date_fmt) %>%
    lubridate::as_date(format = "X%Y%m%d")

  names(r) <- paste(descriptor, 1:terra::nlyr(r), sep = "_")
  return(r)
}

frost_days <- function(r) {
  m <- c(-Inf, 0, 1,
         0, Inf, 0)
  m <- matrix(m, ncol=3, byrow=TRUE)

  terra::classify(r, m, include.lowest=TRUE) %>%
    terra::tapp(index = "years", fun = "sum") %>%
    assign_time_and_name(TRUE, "frost_days")
}

days_above_90 <- function(r, is.kelvin=TRUE) {

  # r <- subset_to_reference(r, reference_period)

  if (is.kelvin) r <- deg_c_to_f(r - 273.15)
  r[r < 90] = 0
  r[r >= 90] = 1

  terra::tapp(r, index="yearmonths", fun="sum") %>%
    assign_time_and_name(FALSE, "abv90")
}

freeze_free_days <- function(r, is.kelvin=TRUE) {

  if (is.kelvin) r <- r - 273.15
  r[r <= 0] = 0
  r[r > 0] = 1

  terra::tapp(r, index="yearmonths", fun="sum") %>%
    assign_time_and_name(FALSE, "freeze-free")
}


wet_days <- function(r, is.base.units=TRUE) {

  if (is.base.units) {
    r <- (r * 86400)/24
  }

  r[r < 1] <- 0
  r[r >= 1] <- 1

  r
}

dry_days <- function(r, is.base.units=TRUE) {

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

growing_season <- function(x, dates = NULL) {

  if (all(is.na(x))) {
    return(NA)
  }

  if (is.null(dates)) {
    dates <- lubridate::as_date(names(x))
  }

  out <- tibble::as_tibble(x) %>%
    tidyr::pivot_longer(
      dplyr::everything()
    ) %>%
    dplyr::mutate(
      date = dates,
      end_date = as.Date(glue::glue("{lubridate::year(date)}-07-01")),
      gt_5 = ifelse(value > 5, 1, 0),
      gt_5_cnt = sequence(rle(gt_5)$lengths),
      start_criteria = ifelse(gt_5 == 1 & gt_5_cnt >= 6, 1, 0),
      end_criteria = ifelse(gt_5 == 0 & gt_5_cnt >= 6 & date >= end_date, 1, 0)
    )  %>%
    dplyr::summarise(
      start =  purrr::detect_index(start_criteria, ~ .x == 1) + 1,
      end =  purrr::detect_index(end_criteria, ~ .x == 1) - 5
    ) %$%
    magrittr::subtract(end, start)

  return(out)

}

calc_growing_season <- function(r, shp) {

  names(r) <- terra::time(r)
  r %>%
    terra::crop(shp, mask=TRUE) %>%
    terra::mask(shp) %>%
    terra::global(fun = "mean", na.rm = T) %>%
    tibble::rownames_to_column() %>%
    magrittr::set_colnames(c("date", "value")) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::group_by(year = lubridate::year(date)) %>%
    dplyr::summarise(growing_season = growing_season(x = value, dates=date))

  # terra::as.points(r) %>%
  #   data.frame(terra::values(.), terra::geom(.)) %>%
  #   dplyr::select(-c(geom, part, hole)) %>%
  #   tibble::as_tibble() %>%
  #   tidyr::pivot_longer(-c(x, y)) %>%
  #   dplyr::mutate(date = as.Date(name, format = "X%Y.%m.%d")) %>%
  #   dplyr::group_by(x, y) -> test
  #
  # dplyr::summarise(test, out = growing_season(x = value, dates = date)) -> a
  #
  # terra::app(r, fun = growing_season) -> test
  # terra::tapp(r, fun = growing_season, index = "years") -> test
}

warm_days <- function(x) {
  sum(ifelse(x > 25, 1, 0))
}

cool_days <- function(x) {
  # Use minimum temperature as input for 'cool days', max temp for 'icing days'
  sum(ifelse(x < 0, 1, 0))
}




temp_vars = c("FD", "SU", "ID", "TR", "GSL", "TXx", "TNx", "TXn", "TNn")
pr_vars = c("Rx1day", "Rx5day", "R10mm", "R20mm", "CDD", "CWD")
