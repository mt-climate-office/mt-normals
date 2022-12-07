#' fetch_nclimgrid
#'
#' @description Download daily nClimGrid data from NOAA.
#'
#' @param data_dir Path to the directory where data will be saved.
#' @param start_year The year to start downloading data from. Defaults to 1951,
#' the earliest year daily data are available for.
#' @param end_year The year to end data downloads for. Defaults to 2015.
#' @param variables A vector of variables to download. Options are
#' c("tmin", "tmax", "tavg", "prcp")
#' @param cores Number of cores to use if you want to parallelize downloads.
#' If left as null, a single core will be used.
#'
#' @return A `tibble::tibble()` with paths to each of the new nclimgrid rasters.
#' @export
#'
#' @examples
#' \dontrun{
#' fetch_nclimgrid("~/data/nclimgrid", 2010, 2011, variables = c("tmin", "tmax"))
#' }
fetch_nclimgrid <- function(
    data_dir,
    start_year=1950,
    end_year=2015,
    variables = c("tmin", "tmax", "tavg", "prcp"),
    cores = NULL
) {

  if (!is.null(cores)) {
    cluster <- multidplyr::new_cluster(10)
    multidplyr::cluster_library(cluster, c("magrittr", "FedData"))
  }

  base_url <- "https://noaa-nclimgrid-daily-pds.s3.amazonaws.com/beta/by-month"
  dat <- tidyr::crossing(
    year = start_year:end_year,
    month = 1:12 %>%
      stringr::str_pad(2, "left", "0"),
    v = variables
  ) %>%
    dplyr::mutate(
      url = glue::glue("{base_url}/{year}/{month}/{v}-{year}{month}-grd-scaled.nc"),
      data_dir = data_dir
    ) %>%
    dplyr::rowwise()

  if (!is.null(cores)) {
    dat %<>%
      multidplyr::partition(cluster) %>%
      dplyr::mutate(
        dest = FedData::download_data(
          url, destdir = data_dir, nc = TRUE, verbose = FALSE, progress=TRUE
        )
      )
  } else {
    dat %<>%
      dplyr::mutate(
        dest = FedData::download_data(
          url, destdir = data_dir, nc = TRUE, verbose = FALSE, progress=TRUE
        )
      )
  }

  return(dat)
}

stack_nclimgrid <- function(data_dir, out_dir, pattern) {

  list.files(data_dir, full.names = T, pattern = pattern) %>%
    terra::rast() %>%
    terra::writeRaster(
      file.path(out_dir, glue::glue("{pattern}_nclimgrid_daily.tif"))
    )
}
