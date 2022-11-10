summarise_by_region <- function(r, shp, attr_id, fun, start_year, end_year, time_as_name=TRUE) {
  r %>%
    {
      if (time_as_name) {
        out <- magrittr::set_names(., terra::time(.))
      } else {
        out <- .
      }
      out
    } %>%
    spat_summary(shp, attr_id = attr_id, fun=fun, name_to = "date") %>%
    dplyr::select(date, !!rlang::sym(attr_id), value) %>%
    {
      if (time_as_name) {
        out <- dplyr::mutate(
          .,
          date = as.Date(date),
          year = lubridate::year(date)
        ) %>%
          dplyr::filter(year %in% start_year:end_year)
      } else {
        out <- .
      }
    }
}

make_trend_string <- function(r, shp, attr_id, fun="mean", start_year=1951, end_year=2015) {

  summarise_by_region(r, shp, attr_id, fun, start_year, end_year) %>%
    dplyr::group_by(!!rlang::sym(attr_id)) %>%
    dplyr::summarise(
      trend = lm(value ~ year) %>%
        coefficients() %>%
        magrittr::extract('year') %>%
        magrittr::multiply_by(10) %>%
        round(2) %>%
        paste("ºF per decade")
    )
}

#' make_trend_fig
#'
#' @param r A timeseries `terra::rast` of an annual climate variable.
#' @param shp An `sf` object with regions to summarise `r` by.
#' @param attr_id The column in `shp` to use as a name.
#' @param fun The function to summarise each region in `shp` by. Defaults to "mean".
#' @param start_year The year to filter the start or the data by.
#' @param end_year The year to filter the end of the data by.
#' @param ylab The label to use for the y-axis of the figure.
#'
#' @return A `ggplot2` figure of change in temperature per decade per region in shp.
#' @export
#'
#' @examples
#' \dontrun{
#' 1+1
#' }
make_trend_fig <- function(r, shp, attr_id, fun="mean", start_year=1951, end_year=2015, ylab="") {

  trends <- make_trend_string(r, shp, attr_id, fun, start_year, end_year)

  summarise_by_region(r, shp, attr_id, fun, start_year, end_year) %>%
    dplyr::left_join(trends, by = "Division") %>%
    dplyr::mutate(
      loc = !!rlang::sym(attr_id),
      lab = glue::glue("{loc} - {trend}")
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x=date, y=value)) +
    ggplot2::geom_line() +
    ggplot2::geom_smooth(method = "lm") +
    ggplot2::facet_wrap(~lab, ncol = 1, scales = "free_y") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x="", y=ylab)
}

trend_func <- function(xs, ys, threshold = 0.5) {

  mod <- lm(ys ~ xs)
  coeffs <- coef(mod)

  meta <- summary(mod)
  if (meta$coefficients[,4][2] > threshold) {
    return(0)
  }
  coeffs[2]
}

#' make_seasonal_trend_table
#'
#' @param r A timeseries `terra::rast` of an annual climate variable.
#' @param shp An `sf` object with regions to summarise `r` by.
#' @param attr_id The column in `shp` to use as a name.
#' @param fun The function to summarise each region in `shp` by. Defaults to "mean".
#' @param start_year The year to filter the start or the data by.
#' @param end_year The year to filter the end of the data by.
#'
#' @return A table giving the decadal rate of change of the climate variable.
#' @export
#'
#' @examples
#' \dontrun{
#' 1+1
#' }
make_seasonal_trend_table <- function(r, shp, attr_id, fun, start_year, end_year) {

  seasons <- group_seasonally(r, start_year, end_year)

  by_season_year <- terra::tapp(
    terra::subset(r, which(terra::time(r) %in% seasons$date)),
    index = seasons$yearly_grp,
    fun = fun
  )

  dat <- summarise_by_region(by_season_year, shp, attr_id, "mean", start_year, end_year, FALSE)

  dat %>%
    tidyr::separate(date, c("season", "year")) %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::group_by(season, !!rlang::sym(attr_id)) %>%
    dplyr::summarise(trend = trend_func(year, value) * 10) %>%
    tidyr::pivot_wider(names_from = season, values_from = trend)
}
