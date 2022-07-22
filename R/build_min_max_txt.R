library(magrittr)

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


build_legend_txt <- function(normals_dir) {
  
  f_list <- list.files(normals_dir, full.names = T, recursive = TRUE) %>% 
    grep("quantiles", ., value = T, invert = T) %>% 
    grep("/maps/", ., value = T, invert = T)
  
  # Iterate over all possibilities and write a json file with each min/max value. 
  tidyr::crossing(
    variables = unique(basename(dirname(f_list))) %>% 
      grep("quantiles|normals", ., invert=T, value = T),
    period = c('annual', tolower(month.abb)),
    what = c("alpha", "beta", "mean", "median", "mode", "variance")
  ) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(j = build_list(f_list, variables, period, what)) %>%
    {
      jsonlite::toJSON(
        .$j, auto_unbox = TRUE, pretty = TRUE
      ) %>%
        jsonlite::write_json(file.path(normals_dir, "legend_data.json"))
    }
  
}
