library(magrittr)

list.files("~/data/cmip_tables/", full.names = T) %>%
  purrr::map(rea)

readr::read_csv( "/Users/Colin.Brust/data/cmip_tables//con-wet_huc_10070008_timeseries_TRUE.csv")
smart_read <- function(x) {
  meta <- basename(x) %>%
    tools::file_path_sans_ext() %>%
    stringr::str_split_1("_") %>%
    magrittr::set_names(c("drop1", "type", "id", "plot_type", "drop2"))

  end <- lubridate::today() %>%
    lubridate::year() - 1

  start = end -29

  dat <- readr::read_csv(x, show_col_types=FALSE) %>%
    tibble::add_column(!!!as.list(meta)) %>%
    dplyr::select(-dplyr::starts_with("drop"))

  if (meta$plot_type == "monthly") {
    dat %>%
      dplyr::group_by(month)
  } else {
    avg <- dplyr::filter(dat, year >= start, year <= end) %>%
      dplyr::pull(value) %>%
      mean()
  }

  dat %>%
    dplyr::mutate(avg = avg)
}

readr::read_csv(x)
