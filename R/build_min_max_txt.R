library(magrittr)

f_list <- list.files("~/data/normals", full.names = T, recursive = TRUE)

# Function to get a list of all min and max value for each dataset.
build_list <- function(f_list, v, period, what) {
  f_list %>%
    grep(glue::glue("/{v}/"), ., value = T) %>%
    grep(period, ., value = T) %>%
    grep(what, ., value = T) %>% 
    terra::rast() %>%
    terra::values() %>%
    {list("min"=min(., na.rm = T), "max"=max(., na.rm = T))}  %>% 
    list() %>%
    magrittr::set_names(glue::glue("{v}-{what}-{period}"))
}

# Iterate over all possibilities and write a json file with each min/max value. 
tidyr::crossing(
  variables = c('pr', 'tmmn', 'tmmx', 'erc', 'rmax', 'rmin', 'sph', 'srad', 'vpd', 'vs'),
  period = c('annual', tolower(month.abb)),
  what = c("quantiles", "alpha", "beta", "mean", "median", "mode", "variance")
) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(j = build_list(f_list, variables, period, what)) %>%
  {
    jsonlite::toJSON(
      .$j, auto_unbox = TRUE, pretty = TRUE
    ) %>%
      jsonlite::write_json("~/data/normals/legend_data.json")
  }



