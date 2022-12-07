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

calc_growing_season <- function(r, shp) {

  # Crop to ROI
  r %<>%
    terra::crop(normals::mt) %>%
    terra::mask(normals::mt)

  # Make a raster that is the Julian day of the year.
  day <- terra::deepcopy(r)
  terra::values(day) <- terra::time(r) %>%
    lubridate::yday() %>%
    rep(each = terra::ncell(r))

  # If before July 1st, set to 0
  day <- terra::classify(
    day,
    matrix(
      c(-Inf, 182, 0,
        182, Inf, 1),
      ncol=3, byrow=TRUE
    ),
    include.lowest=TRUE)

  lt_5 <- terra::classify(
    r,
    matrix(
      c(-Inf, 5, 1,
        5, Inf, 0),
      ncol=3, byrow=TRUE
    ),
    include.lowest=FALSE) %>%
    terra::roll(n = 6, fun = "prod", type="from", circular = TRUE, na.rm = T)

  lt_5 <- terra::app(lt_5 * day, fun = "which.max")

  gt_5 <- terra::classify(
    r,
    matrix(
      c(-Inf, 5, 0,
        5, Inf, 1),
      ncol=3, byrow=TRUE
    ),
    include.lowest=FALSE) %>%
    terra::roll(n = 6, fun = "prod", type="from", circular = TRUE, na.rm = T) %>%
    terra::app(fun = "which.max")

  growing_season <- lt_5 - gt_5
  names(growing_season) <- "Growing Season"

  year <- r %>%
    terra::time() %>%
    lubridate::year() %>%
    unique() %>%
    head(1) %>%
    paste("-01-01") %>%
    lubridate::as_date()

  terra::time(growing_season) <- year
  return(growing_season)
}

warm_days <- function(x) {

  sum(ifelse(x > 25, 1, 0))
}

cool_days <- function(x) {
  # Use minimum temperature as input for 'cool days', max temp for 'icing days'
  sum(ifelse(x < 0, 1, 0))
}

climdex_from_raw <- function(raw_dir, out_dir) {

  dat <- list.files(raw_dir, full.names = T) %>%
    tibble::tibble(f = .) %>%
    dplyr::mutate(name = basename(f)) %>%
    tidyr::separate(name, c("variable", "date", "drop1", "drop2"), sep = "-") %>%
    dplyr::mutate(date = paste0(date, "01") %>%
                    as.Date(format = "%Y%m%d")) %>%
    dplyr::select(-dplyr::starts_with("drop")) %>%
    dplyr::filter(lubridate::year(date) <= 1953)

  growing_season <- dat %>%
    dplyr::filter(variable == "tavg") %>%
    dplyr::group_by(year = lubridate::year(date)) %>%
    dplyr::summarise(r = list(
      calc_growing_season(r = terra::rast(f), shp = normals::mt)
    )) %>%
    dplyr::summarise(r = list(terra::rast(r)), name = "growing_season_annual.tif")
}
list.files("~/data/nclimgrid/", pattern = "")

temp_vars = c("FD", "SU", "ID", "TR", "GSL", "TXx", "TNx", "TXn", "TNn")
pr_vars = c("Rx1day", "Rx5day", "R10mm", "R20mm", "CDD", "CWD")