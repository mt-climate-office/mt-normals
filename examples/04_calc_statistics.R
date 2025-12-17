library(magrittr)

dat <- arrow::open_dataset("~/data/zonal") %>%
  dplyr::collect()


calc_mean <- function(x) {

  s <- lubridate::year(lubridate::today()) - 1
  e <- s - 29

  out <- x %>%
    dplyr::filter(
      lubridate::year(date) >= e,
      lubridate::year(date) <= s
    ) %>%
    dplyr::group_by(time = base::tolower(base::month.abb[lubridate::month(date)])) %>%
    dplyr::summarise(
      mean = base::mean(value, na.rm = TRUE),
      p25 = stats::quantile(value, 0.25, na.rm = TRUE),
      p75 = stats::quantile(value, 0.75, na.rm = TRUE)
    )

  tibble::tribble(
    ~time, ~mean, ~p25, ~p75,
    "annual",
    base::mean(out$mean, na.rm = TRUE),
    stats::quantile(out$mean, 0.25, na.rm = TRUE),
    stats::quantile(out$mean, 0.75, na.rm = TRUE)
  ) %>%
    dplyr::bind_rows(out)

}

calc_trends <- function(x) {

  summary_df <- x %>%
    dplyr::mutate(
      year = lubridate::year(date),
      month = lubridate::month(date, label = TRUE)
    ) %>%
    dplyr::arrange(date) %>%
    dplyr::group_by(month) %>%
    dplyr::do({
      fit <- stats::lm(value ~ year, data = .)
      tibble::tibble(
        trend = stats::coef(fit)[2],
        p_value = broom::tidy(fit)$p.value[2]
      )
    }) %>%
    dplyr::ungroup() %>%
    dplyr::rename(time = month) %>%
    dplyr::mutate(time = tolower(time))

  annual_trend <- x %>%
    dplyr::mutate(year = lubridate::year(date)) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(mean_value = base::mean(value, na.rm = TRUE)) %>%
    dplyr::do({
      fit <- stats::lm(mean_value ~ year, data = .)
      tibble::tibble(
        trend = stats::coef(fit)[2],
        p_value = broom::tidy(fit)$p.value[2]
      )
    }) %>%
    dplyr::mutate(time = "annual")

  dplyr::bind_rows(summary_df, annual_trend)
}

stats <- dat %>%
  dplyr::group_by(id, type, variable) %>%
  dplyr::group_split() %>%
  purrr::map(
    \(x) {
      m = calc_mean(x)
      t = calc_trends(x)
      x %>%
        dplyr::select(name, type, id, variable) %>%
        dplyr::distinct() %>%
        dplyr::cross_join(dplyr::left_join(m, t, "time"))

    }
  ) %>%
  dplyr::bind_rows()

stats %>%
  arrow::write_parquet("~/data/stats.parquet")
