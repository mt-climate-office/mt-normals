#' group_seasonally
#'
#' @description Create a table giving seasons for each raster layer in a `terra::rast` timeseries.
#'
#' @param r The `terra::rast` timeseries to create the table for.
#' @param start_year The start year to filter the timeseries by. Any years prior
#' to this year will be removed
#' @param end_year The end year to filter the timeseries by. Any years after this
#' year will be removed.
#'
#' @return A [tibble::tibble()] giving the seasons of each layer in a raster
#' @export
#'
#' @examples
#' \dontrun{
#' # fill with example
#' 1+1
#' }
group_seasonally <- function(r, start_year, end_year) {
  tibble::tibble(
    date = terra::time(r)
  ) %>%
    dplyr::mutate(
      season = dplyr::case_when(
        lubridate::month(date) %in% c(12, 1, 2) ~ "Winter", # 1=winter
        lubridate::month(date) %in% c(3, 4, 5) ~ "Spring", # 2=spring
        lubridate::month(date) %in% c(6, 7, 8) ~ "Summer", # 3=summer
        lubridate::month(date) %in% c(9, 10, 11) ~ "Fall" # 4=fall
      ),
      idx = 1:dplyr::n(),
      month = lubridate::month(date)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      year = ifelse(
        (season == "Winter") && (lubridate::month(date)) == 12,
        lubridate::year(date) + 1,
        lubridate::year(date)
      )
    ) %>%
    dplyr::filter(year >= start_year) %>%
    dplyr::filter(year <= end_year) %>%
    dplyr::group_by(year, season) %>%
    dplyr::mutate(
      yearly_grp = dplyr::cur_group_id(),
      group_count = dplyr::n()
    ) %>%
    dplyr::mutate(
      yearly_grp = glue::glue("{season} {year}")
    ) %>%
    dplyr::filter(group_count == 3)
}

#' make_temp_seasonal_rasters
#'
#' @param tmmn A `terra::rast` timeseries of minimum temperature.
#' @param tmmx A `terra::rast` timeseries of maximum temperature.
#' @param out_dir The directory to save the seasonal rasters out to.
#' @param start_year The start year of the reference period to filter input data by.
#' @param end_year The end year of the reference period to filter input data by.
#'
#' @return Results are saved to an output directory.
#' @export
#'
#' @examples
#' \dontrun{
#' # fill with example
#' 1+1
#' }
make_temp_seasonal_rasters <- function(tmmn, tmmx, out_dir, start_year, end_year) {
  tmmx <- filter_to_reference(tmmx, start_year-1, end_year)
  tmmn <- filter_to_reference(tmmn, start_year-1, end_year)
  tavg <- (tmmx + tmmn)/2

  tmmx <- terra::tapp(
    tmmx,
    index = group_seasonally(tmmx, start_year, end_year) %$% season,
    fun = "mean"
  )

  terra::writeRaster(tmmx, file.path(out_dir, "tmmx_historical_seasonal.tif"), overwrite = T)

  tmmn <- terra::tapp(
    tmmn,
    index = group_seasonally(tmmn, start_year, end_year) %$% season,
    fun = "mean"
  )

  terra::writeRaster(tmmn, file.path(out_dir, "tmmn_historical_seasonal.tif"), overwrite = T)

  tavg <- terra::tapp(
    tavg,
    index = group_seasonally(tavg, start_year, end_year) %$% season,
    fun = "mean"
  )

  terra::writeRaster(tavg, file.path(out_dir, "tavg_historical_seasonal.tif"), overwrite = T)
  return(NA)
}

#' make_pr_seasonal_raster
#'
#' @param pr A `terra::rast` timeseries of monthly precipitation totals.
#' @param out_dir The directory to save the seasonal rasters out to.
#' @param start_year The start year of the reference period to filter input data by.
#' @param end_year The end year of the reference period to filter input data by.
#'
#' @return Results are saved to an output directory.
#' @export
#'
#' @examples
#' \dontrun{
#' # fill with example
#' 1+1
#' }
make_pr_seasonal_raster <- function(pr, out_dir, start_year, end_year) {
  pr <- filter_to_reference(pr, start_year-1, end_year)
  indices <- group_seasonally(pr, start_year, end_year)

  pr <- terra::tapp(
    pr,
    index = indices$yearly_grp,
    fun = "sum"
  )

  new_idx <- names(pr) %>%
    stringr::str_split(pattern = "[.]") %>%
    lapply(utils::head, 1) %>%
    unlist()

  pr <- terra::tapp(
    pr,
    index = new_idx,
    fun = "mean"
  )

  terra::writeRaster(pr, file.path(out_dir, "pr_historical_seasonal.tif"), overwrite = T)
}
