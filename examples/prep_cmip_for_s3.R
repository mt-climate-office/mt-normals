library(magrittr)

dat <- list.files("~/data/cmip", full.names = T, pattern = ".tif") %>%
  grep(".json", ., value = T, invert = T) %>%
  tibble::tibble(f = .) %>%
  dplyr::mutate(bn = basename(f) %>%
                  tools::file_path_sans_ext()) %>%
  tidyr::separate(bn, c("model", "scenario", "drop", "variable"), sep = "_") %>%
  dplyr::select(-drop)

historical <- dat %>%
  dplyr::filter(time == "scenario")

dat %>%
  dplyr::filter(time != "scenario")


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

    dat %>%
      dplyr::mutate(avg = avg) %>%
      return()
  }


}

readr::read_csv(x)
