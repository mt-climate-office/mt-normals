library(normals)
library(magrittr)


crop_daily_ncs <- function(data_dir) {

  list.files(file.path(data_dir, "raw"), full.names = T, recursive = TRUE, pattern = ".nc") %>%
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


calc_summaries_from_daily <- function(data_dir = "~/data/gridmet") {

  monthlys <- file.path(data_dir, "montana") %>%
    list.files(include.dirs = T, recursive = T, pattern = ".tif") %>%
    grep("aggregated", ., value = T, invert = T) %>%
    tibble::tibble(f = .) %>%
    tidyr::separate(f, c("variable", "date", "d1", "ext"), remove = F) %>%
    dplyr::select(-d1, -ext) %>%
    dplyr::mutate(
      f = file.path(data_dir, "montana", f),
      date = lubridate::as_date(date),
      func = ifelse(variable %in% c("pr", "etr", "pet"), "sum", "mean")
    ) %>%
    dplyr::group_by(year = lubridate::year(date),
                    month = lubridate::month(date),
                    variable) %>%
    dplyr::summarise(
      r = list({
        yr <- dplyr::first(year)
        mo <- dplyr::first(month)
        vr <- dplyr::first(variable)
        terra::rast(f) %>%
          normals::aggregate_daily(
            dplyr::first(variable),
            TRUE,
            dplyr::first(func),
            file.path(dirname(dplyr::first(f)), "aggregated", glue::glue("monthly_{yr}{stringr::str_pad(mo, 2, pad = '0')}01_{vr}.tif"))
          )
      }),
      func = dplyr::first(func)

    )

  annuals <- monthlys %>%
    dplyr::group_by(year, variable) %>%
    dplyr::summarise(
      r = list(terra::rast(r)  %>%
                 normals::aggregate_daily(
                   dplyr::first(variable),
                   FALSE,
                   dplyr::first(func),
                   file.path(data_dir, "montana", dplyr::first(variable), "aggregated", glue::glue("annual_{dplyr::first(year)}0101_{dplyr::first(variable)}.tif"))
                 ))
    )
}

calc_normals <- function(data_dir = "~/data/gridmet/montana") {
  normal_year = lubridate::today() %>%
    lubridate::year() %>%
    magrittr::subtract(1)

  start_year = normal_year - 29

  list.files(data_dir, full.names = T, include.dirs = T) %>%
    file.path("normals", glue::glue("{start_year}-{normal_year}")) %>%
    purrr::map(dir.create, recursive = T)

  list.files(data_dir, full.names = T, recursive = T) %>%
    stringr::str_subset("aggregated") %>%
    tibble::tibble(pth = .) %>%
    dplyr::mutate(
      f = pth %>%
        basename() %>%
        tools::file_path_sans_ext()
    ) %>%
    tidyr::separate(f, c("time", "date", "variable"), sep = "_") %>%
    dplyr::mutate(date = lubridate::as_date(date),
                  month = lubridate::month(date)) %>%
    dplyr::filter(lubridate::year(date) >= start_year, lubridate::year(date) <= normal_year) %>%
    dplyr::group_by(time, variable, month) %>%
    dplyr::mutate(
      tname = dplyr::case_when(
        time == "monthly" ~ tolower(month.abb[month]),
        .default = "annual"
      )
    ) %>%
    dplyr::summarise(
      r = list(
        terra::rast(pth) %>%
           normals::gamma_from_rast(
             out_dir = file.path(data_dir, dplyr::first(variable), "normals", glue::glue("{start_year}-{normal_year}")),
             descriptor = glue::glue("{dplyr::first(tname)}_{dplyr::first(variable)}")
           )
      )
    )
}

data_dir = "~/data/gridmet"
normals::fetch_gridmet(data_dir, c("erc", "etr", "pet", "pr", "rmax", "rmin", "sph", "srad", "th", "tmmn", "tmmx", "vpd", "vs"),
                       start_year = 1979, end_year = 2025, force_dl = F)
crop_daily_ncs(data_dir)
calc_summaries_from_daily(data_dir)
calc_normals(file.path(data_dir, "montana"))
