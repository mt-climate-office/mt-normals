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
#' @param transform A unit transform function to apply to the daily data before
#' they are aggregated.
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
aggregate_daily <- function(r, variable, monthly = TRUE, agg_func = "mean", filename=NULL, transform = NA, ...) {

  if (!is.na(transform)) {
    r <- transform(r)
  }

  grp <- ifelse(monthly, "yearmonths", "years")
  out <- terra::tapp(r, grp, fun = agg_func, na.rm = T)

  terra::time(out) <- names(out) %>%
    stringr::str_replace("X", "") %>%
    paste0(
      ifelse(monthly, "01", "0101")
    ) %>%
    lubridate::as_date(format = "%Y%m%d")

  terra::set.names(out, glue::glue("{1:terra::nlyr(out)}_{variable}"))
  if (!is.null(filename)) {
    terra::writeRaster(out, filename, ...)
  }
  return(out)
}
