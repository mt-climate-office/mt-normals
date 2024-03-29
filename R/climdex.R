assign_time_and_name <- function(r, annual, descriptor) {

  date_fmt <- ifelse(annual, "0101", "01")
  terra::time(r) <- names(r) %>%
    paste0(date_fmt) %>%
    as.Date(format = "X%Y%m%d")

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



convert_reclassify_gdd <-function(x, low, high) {
  x <- ((x - 273.15) * (9/5) + 32)
  terra::clamp(x, low, high)
}

calc_gdds <- function(tmmx, tmmn, shp, low_thresh=50, high_thresh=86) {
  print('calculating gdd')
  tmmx <- convert_reclassify_gdd(tmmx, low_thresh, high_thresh)
  tmmn <- convert_reclassify_gdd(tmmn, low_thresh, high_thresh)

  tavg <- (tmmx + tmmn) / 2
  gdds <- tavg - low_thresh

  return(gdds)
}

write_gdd_data <- function(daily_dir="~/data/cmip6/daily/", monthly_dir="~/data/cmip6/monthly/") {
  gdds <- list.files(daily_dir, full.names = T) %>%
    stringr::str_subset(".json", negate = T) %>%
    stringr::str_subset("tasmax|tasmin") %>%
    tibble::tibble(r = .) %>%
    dplyr::mutate(base = basename(r) %>%
                    tools::file_path_sans_ext()) %>%
    tidyr::separate(base, c("model", "period", "conditions", "variable"), sep = "_") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(r = list(terra::rast(r))) %>%
    tidyr::pivot_wider(names_from = variable, values_from = r)

  gdds %<>%
    dplyr::rowwise() %>%
    dplyr::mutate(gdd = list(calc_gdds(tasmax, tasmin)))

  gdds %>%
    dplyr::mutate(
      name = file.path(
        paste(c(model, period, conditions, "gdd.tif"), collapse = "_")
      ),
      gdd = list(normals::write_as_cog(gdd, file.path(daily_dir, name))),
      monthly = list(
        terra::tapp(gdd, index = "yearmonths", fun = "sum") %>%
          normals::write_as_cog(file.path(monthly_dir, name))
      )
    )

}


calc_growing_season <- function(r, shp, year) {
  print(glue::glue("Processing Growing Season for {year}"))
  # Crop to ROI
  r %<>%
    terra::crop(shp) %>%
    terra::mask(shp)

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

  year <- as.Date(glue::glue("{year}-01-01"))

  terra::time(growing_season) <- year
  return(growing_season)
}

calc_warm_days <- function(r, shp, year) {
  print(glue::glue("Processing Warm Days for {year}"))
  r %<>%
    terra::crop(shp) %>%
    terra::mask(shp)

  terra::classify(
    r,
    matrix(
      c(-Inf, 25, 0,
        25, Inf, 1),
      ncol=3, byrow=TRUE
    ),
    include.lowest=FALSE) %>%
    terra::app(fun="sum") %>%
    terra::`time<-`(as.Date(glue::glue("{year}-01-01"))) %>%
    `names<-`("Warm Days")
}

calc_cool_days <- function(r, shp, year, name) {
  print(glue::glue("Processing Cool Days for {year}"))
  # Use minimum temperature as input for 'cool days', max temp for 'icing days'
  r %<>%
    terra::crop(shp) %>%
    terra::mask(shp)

  terra::classify(
    r,
    matrix(
      c(-Inf, 0, 1,
        0, Inf, 0),
      ncol=3, byrow=TRUE
    ),
    include.lowest=FALSE) %>%
    terra::app(fun="sum") %>%
    terra::`time<-`(as.Date(glue::glue("{year}-01-01"))) %>%
    `names<-`(name)
}

calc_period_differences <- function(r, start, end, fun=median) {

  years <- terra::time(r)
  start_r <- terra::subset(
    r, which(lubridate::year(years) %in% start[1]:start[2])
  ) %>%
    terra::app(fun = "median")

  end_r <- terra::subset(
    r, which(lubridate::year(years) %in% end[1]:end[2])
  ) %>%
    terra::app(fun = "median")


  diff <- end_r - start_r

  tibble::tibble(
    period = c("start", "end", "diff"),
    r = list(start_r, end_r, diff)
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      val = terra::global(r, fun = fun, na.rm = T),
      start_r = list(start_r),
      end_r = list(end_r),
      diff = list(diff)
    ) %>%
    tidyr::unnest(cols = c(val)) %>%
    dplyr::select(period, value=global, start_r, end_r, diff)
}


make_extremes_table <- function(dat) {
  dat %>%
    dplyr::mutate(xtreme = list(calc_period_differences(r, c(1951, 1980), c(1991, 2020)))) %>%
    dplyr::select(name, xtreme) %>%
    tidyr::unnest(cols = xtreme) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      start_r = list(terra::wrap(start_r)),
      end_r = list(terra::wrap(end_r)),
      diff = list(terra::wrap(diff))
    )
}

apply_func_from_dat <- function(dat, fun, filt, out_name, ...) {
  dat %>%
    dplyr::filter(variable == filt) %>%
    dplyr::group_by(year = lubridate::year(date)) %>%
    dplyr::summarise(r = list(
      fun(r = terra::rast(f), shp = shp, year = dplyr::cur_group(), ...)
    )) %>%
    dplyr::summarise(r = list(terra::rast(r)), name = out_name)
}

climdex_from_raw <- function(raw_dir, out_dir, shp) {

  dat <- list.files(raw_dir, full.names = T) %>%
    tibble::tibble(f = .) %>%
    dplyr::mutate(name = basename(f)) %>%
    tidyr::separate(name, c("variable", "date", "drop1", "drop2"), sep = "-") %>%
    dplyr::mutate(date = paste0(date, "01") %>%
                    as.Date(format = "%Y%m%d")) %>%
    dplyr::select(-dplyr::starts_with("drop"))

  growing_season <- apply_func_from_dat(
    dat, calc_growing_season, "tavg", "growing_season.tif"
  )

  warm_days <- apply_func_from_dat(
    dat, calc_warm_days, "tmax", "warm_days.tif"
  )

  cool_days <-  apply_func_from_dat(
    # Change to tmin
    dat, calc_cool_days, "tmin", "cool_days.tif", name="cool_days"
  )

  icing_days <- apply_func_from_dat(
    # change to tmax
    dat, calc_cool_days, "tmax", "icing_days.tif", name="icing_days"
  )

  monthly_max <- dat %>%
    dplyr::filter(
      variable == "tmax"
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(r = terra::rast(f) %>%
                    terra::crop(shp) %>%
                    terra::mask(shp) %>%
                    terra::app(fun="mean") %>%
                    list()) %>%
    dplyr::group_by(year = lubridate::year(date)) %>%
    dplyr::summarise(r = terra::rast(r) %>%
                       terra::app(fun="max") %>%
                       terra::`time<-`(glue::glue("{dplyr::cur_group()}-01-01") %>% as.Date()) %>%
                       list()) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(r = list(terra::rast(r))) %>%
    dplyr::mutate(name = "monthly_max.tif")

  monthly_min <- dat %>%
    dplyr::filter(
      variable == "tmin"
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(r = terra::rast(f) %>%
                    terra::crop(shp) %>%
                    terra::mask(shp) %>%
                    terra::app(fun="mean") %>%
                    list()) %>%
    dplyr::group_by(year = lubridate::year(date)) %>%
    dplyr::summarise(r = terra::rast(r) %>%
                       terra::app(fun="min") %>%
                       terra::`time<-`(glue::glue("{dplyr::cur_group()}-01-01") %>% as.Date()) %>%
                       list()) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(r = list(terra::rast(r))) %>%
    dplyr::mutate(name = "monthly_min.tif")


  dplyr::bind_rows(
    growing_season, warm_days, cool_days, icing_days, monthly_max, monthly_min
  ) %>%
    dplyr::rowwise() %>%
    make_extremes_table() %>%
    saveRDS(file.path(out_dir, "climdex.rds"))
}
