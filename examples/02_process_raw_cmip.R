library(magrittr)
f_dir = "~/data/cmip/derived"


append_historical <- function(m, v, r, historical) {

  tmp <- historical %>%
    dplyr::filter(model == !!m,
                  variable == !!v)

  c(tmp$r[[1]], r)
}

calc_monthly_avg <- function(f_dir) {
  list.files(f_dir, full.names = T, include.dirs = F, recursive = F) %>%
    tibble::tibble(f = .) %>%
    dplyr::mutate(bn = basename(f) %>% tools::file_path_sans_ext()) %>%
    tidyr::separate(bn, c("variable", "model", "scenario", "year"), sep = "_") %>%
    dplyr::mutate(out_name = file.path("monthly", glue::glue("{variable}_{model}_{scenario}.tif"))) %>%
    dplyr::group_by(variable, model, scenario) %>%
    dplyr::group_split() %>%
    purrr::map(function(x) {
      out_name = x$out_name[[1]]
      if (file.exists(out_name)) {
        print(glue::glue("{out_name} exists, skipping..."))
        return(out_name)
      }
      print(glue::glue("working on {out_name}"))
      func = ifelse(x$variable[[1]] %in% c("pr", "eto"), "sum", 'mean')
      r <- terra::rast(x$f) %>%
        terra::tapp(index = "yearmonths", fun = func)

      normals::write_as_cog(r, out_name)
      return(out_name)
    })
}


calc_eto <-
  function(x, lat, elev, outfile, force = FALSE){
    outfile <- outfile[[1]]
    if(!force && file.exists(outfile)){
      return(outfile)
    }
    print(glue::glue("working on {outfile}..."))

    tryCatch({
      days <- ETo::get_days_from_raster(x$rsds)
      ETo::etr_penman_monteith(
        lat = lat,
        days = days,
        elev = elev,
        srad = x$rsds,
        t_min = x$tasmin - 273.15,
        t_max = x$tasmax - 273.15,
        rh_mean = terra::clamp(x$hurs, lower = 0, upper = 100),
        ws = x$sfcWind,
        wind_height = 10,
        reference = 0.23
      ) %>%
        terra::clamp(lower = 0) %>%
        normals::write_as_cog(outfile)
    }, error = function(e) {
      message(glue::glue("Error processing {outfile}: {e$message}"))
      return(outfile)
    })
  }

process_eto <- function(f_dir) {
  elev <-
    list.files(f_dir,
               full.names = TRUE,
               pattern = ".tif") %>%
    magrittr::extract2(1) %>%
    terra::rast() %>%
    magrittr::extract2(1) %>%
    terra::rotate() %>%
    {terra::mask(ETo::get_elev_from_raster(., z = 3), .)} %>%
    terra::rotate()

  lat <- list.files(f_dir,
                        full.names = TRUE,
                        pattern = ".tif") %>%
    magrittr::extract2(1) %>%
    terra::rast() %>%
    magrittr::extract2(1) %>%
    terra::rotate() %>%
    ETo::get_lat_from_raster() %>%
    terra::rotate()

  list.files(f_dir,
             full.names = TRUE,
             pattern = ".tif",
             include.dirs = F,
             recursive = F) %>%
    tibble::tibble(rast = .) %>%
    dplyr::mutate(dat =
                    rast %>%
                    basename() %>%
                    tools::file_path_sans_ext()) %>%
    tidyr::separate_wider_delim(dat,
                                names = c("element", "model", "scenario", "year"),
                                delim = "_",
                                cols_remove = FALSE,
                                too_few = "align_start") %>%
    dplyr::filter(element != "eto") %>%
    dplyr::filter(model == "ACCESS-ESM1-5",
                  scenario == "historical",
                  year == 1951) %>%
    dplyr::group_by(model, scenario, year) %>%
    dplyr::arrange(model, scenario, year) %>%
    dplyr::summarise(
      eto =
       list(rast %>%
         purrr::map(terra::rast) %>%
         magrittr::set_names(element) %>%
         calc_eto(
          lat = lat,
          elev = elev,
          outfile = file.path(f_dir, glue::glue("eto_{model}_{scenario}_{year}.tif"))
         ))
    )
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

deg_c_to_f <- function(t) {
  (t * 1.8) + 32
}


calc_derived_metrics <- function(f_dir, out_dir) {
  tmmx <- list.files(f_dir, full.names = T, pattern = "tasmax_")
  tmmn <- list.files(f_dir, full.names = T, pattern = "tasmin_")
  pr <- list.files(f_dir, full.names = T, pattern = "pr")
  future::plan(future::multisession, workers = 4)

  furrr::future_map(tmmx, function(x) {
    print(x)
    r <- terra::rast(x)
    above_90 <- calc_days_above_90(r, TRUE)
    out_name <- basename(stringr::str_replace(x, "tasmax", "above90"))
    out_path <- file.path(out_dir, out_name)
    normals::write_as_cog(above_90, out_path)
  })

  furrr::future_map(tmmn, function(x) {
    print(x)
    r <- terra::rast(x)
    freeze_free <- calc_freeze_free_days(r, TRUE)
    out_name <- basename(stringr::str_replace(x, "tasmin", "freeze-free"))
    out_path <- file.path(out_dir, out_name)
    normals::write_as_cog(freeze_free, out_path)
  })

  furrr::future_map(pr, function(x) {
    print(x)
    r <- terra::rast(x)
    dry <- calc_dry_days(r, TRUE)
    wet <- calc_wet_days(r, TRUE)

    get_consecutive_days(dry) %>%
      assign_time_and_name(TRUE, "con-dry") %>%
      normals::write_as_cog(
        file.path(
          out_dir,
          basename(stringr::str_replace(x, "pr", "con-dry"))
        )
      )

    con_wet <- get_consecutive_days(wet) %>%
      assign_time_and_name(TRUE, "con-wet") %>%
      normals::write_as_cog(
        file.path(
          out_dir,
          basename(stringr::str_replace(x, "pr", "con-wet"))
        )
      )

    terra::tapp(dry, "yearmonths", "sum") %>%
      assign_time_and_name(FALSE, "dry-days") %>%
      normals::write_as_cog(
        file.path(
          out_dir,
          basename(stringr::str_replace(x, "pr", "dry-days"))
        )
      )

    terra::tapp(wet, "yearmonths", "sum") %>%
      assign_time_and_name(FALSE, "wet-days") %>%
      normals::write_as_cog(
        file.path(
          out_dir,
          basename(stringr::str_replace(x, "pr", "wet-days"))
        )
      )
  })
}


calc_monthly_for_derived <- function(){
  list.files("~/data/cmip/derived", full.names = T, include.dirs = F, recursive = F) %>%
    tibble::tibble(f = .) %>%
    dplyr::mutate(bn = basename(f) %>% tools::file_path_sans_ext()) %>%
    tidyr::separate(bn, c("variable", "model", "scenario", "year"), sep = "_") %>%
    dplyr::mutate(out_name = file.path("~/data/cmip/monthly", glue::glue("{variable}_{model}_{scenario}.tif"))) %>%
    dplyr::group_by(variable, model, scenario) %>%
    dplyr::group_split() %>%
    purrr::map(function(x) {
      out_name = x$out_name[[1]]
      if (file.exists(out_name)) {
        print(glue::glue("{out_name} exists, skipping..."))
        return(out_name)
      }
      print(glue::glue("working on {out_name}"))
      func = ifelse(x$variable[[1]] %in% c("pr", "eto"), "sum", 'mean')

      r <- x %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          r = {
            rast <- terra::rast(f)
            n_layers <- terra::nlyr(rast)
            if (n_layers == 1) {
              terra::time(rast) <- as.Date(paste0(year, "-01-01"))
            } else {
              terra::time(rast) <- seq(as.Date(paste0(year, "-01-01")),
                                       as.Date(paste0(year, "-12-01")),
                                       by = "month")
            }
            list(rast)
          }
        ) %>%
        dplyr::pull(r) %>%
        terra::rast()

      normals::write_as_cog(r, out_name)
      return(out_name)
    })
}


