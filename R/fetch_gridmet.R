#' fetch_gridmet
#' @description Create a data directory for creating climate normals and download
#' gridMET data to it for a specified period of record. This function requires
#' `wget` to be globally available on your system.
#'
#' @param data_dir Directory to save data to. If the directory doesn't exist,
#' it will be created. Raw data will be saved to `data_dir/raw`. An additional
#' `data_dir/processed` will be created to save processed data in later.
#' @param variables A character vector of gridMET variables to download, options
#' to choose from are `c("erc",  "etr",  "pet",  "pr",  "rmax",  "rmin",  "sph",  "srad",  "th",  "tmmn",  "tmmx",  "vpd",  "vs")`
#' @param start_year An integer of year to start download data for (minimum possible is 1979).
#' @param end_year An integer of year to end data download (max possible is the current year).
#'
#' @return NA, data are saved to `data_dir/raw`
#' @export
#'
#' @examples
#' \dontrun{
#' fetch_gridmet("~/data_dir", c("tmmx", "tmmn", "pr"), 1991, 2020)
#' }
fetch_gridmet <-
  function(data_dir,
           variables,
           start_year = 1991,
           end_year = 2020) {
    data_dir <- file.path(data_dir, "raw")
    if (!dir.exists(data_dir)) {
      dir.create(data_dir, recursive = T)
    }

    for (v in variables) {
      tmp <- file.path(data_dir, v)
      if (!dir.exists(tmp)) {
        dir.create(tmp)
      }
      for (year in start_year:end_year) {
        f <- glue::glue("{v}_{year}.nc")
        print(glue::glue("Downloading {f}"))
        url_pth <-
          file.path("http://www.northwestknowledge.net/metdata/data", f)
        f_pth <- file.path(tmp, f)
        system(glue::glue('wget -nc -c -nd --quiet --no-verbose -P {tmp} {url_pth}'))
      }
    }

    processed_dir <- file.path(dirname(data_dir), "processed")
    if (!dir.exists(processed_dir)) {
      dir.create(processed_dir)
    }

  }
