library(magrittr)


#' Aggregate Daily data to a monthly or annual timescale.
#'
#' @param r A multilayer `terra::rast` of daily data spanning at least one month.
#' The raster must have a time attributed assigned with `terra::time`.
#' @param variable A string giving the name of the variable being created.
#' @param monthly A boolean specifying whether monthly or annual averages should
#' be computed.
#' @param agg_func The function used to aggregate the daily data. Defaults to
#' "mean"
#' @param filename A path/name to save the file as. If left empty, nothing will
#' be written to disk.
#' @param ... Additional arguments that will be passed to `terra::writeRaster`.
#'
#' @return A `terra::rast` with monthly data.
#' @export
#'
#' @examples
#' # Make example raster
#' r <- matrix(1:100, 10, 10) |>
#'   terra::rast()
#'
#' # repeat 31 times
#' r <-  r[[rep(1, 31)]]
#'
#' # Assign dates
#' terra::time(r) <- seq.Date(
#'   as.Date("2022-01-01"), as.Date("2022-01-31"), by = 'days'
#' )
#'
#' # Calculate the monthly mean.
#' aggregate_daily(r, 'example', TRUE, 'mean')
#' # Make example raster
#' r <- matrix(1:100, 10, 10) |>
#'   terra::rast()
#'
#' # repeat 365 times
#' r <-  r[[rep(1, 365)]]
#'
#' # Assign dates
#' terra::time(r) <- seq.Date(
#'   as.Date("2022-01-01"), as.Date("2022-12-31"), by = 'days'
#' )
#'
#' # Calculate the monthly mean.
#' aggregate_daily(r, 'example', FALSE, 'mean')
aggregate_daily <- function(r, variable, monthly = TRUE, agg_func = "mean", filename=NULL, ...) {
  tm <- terra::time(r)

  if (monthly) {
    mons <- lubridate::month(tm)
  } else {
    mons <- rep(1, terra::nlyr(r))
  }

  groups <- tibble::tibble(date = tm) |>
    dplyr::mutate(
      mon = mons,
      year = lubridate::year(date)
    ) |>
    dplyr::group_by(mon, year) |>
    dplyr::summarise(
      idx = dplyr::cur_group_id(), .groups = "drop"
    ) |>
    dplyr::mutate(
      date = lubridate::as_date(
        glue::glue("{year}-{mon}-01", format="%Y-%m-%d")
      )
    )

  out <- terra::tapp(r, groups$idx, fun = agg_func, na.rm = T)
  terra::time(out) <- groups$date
  terra::set.names(out, glue::glue("{1:terra::nlyr(out)}_{variable}"))
  if (!is.null(filename)) {
    terra::writeRaster(out, filename, ...)
  }
  return(out)
}
