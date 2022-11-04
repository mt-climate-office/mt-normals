group_seasonally <- function(r) {
  tibble::tibble(
    date = names(r) %>%
      stringr::str_replace("X", "") %>%
      lubridate::as_date()
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
    dplyr::group_by(year, season) %>%
    dplyr::mutate(
      yearly_grp = dplyr::cur_group_id(),
      group_count = dplyr::n()
    ) %>%
    dplyr::filter(
      year %in% reference_period[1]:reference_period[2],
    ) %>%
    dplyr::mutate(
      yearly_grp = glue::glue("{season} {year}")
    )
}

make_historical_data <- function(raw_dir, shp, variables) {

}
make_table_by_group <- function(shp, r, attr_id=NULL, fun="mean") {

}
