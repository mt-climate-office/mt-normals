library(magrittr)


# Converts names of raster layer to dates.
name_to_date <- function(x, variable) {
  x %>%
    names() %>%
    lubridate::as_date(format = glue::glue("{variable}_%Y_%j"))
}


subset_period <- function(r,
                          period = "annual",
                          agg_func = "mean") {
  switch(period,
         # Aggregate across everything if period is annual.
         "annual" = {
           terra::app(r, fun = agg_func)
         },
         # Otherwise aggregate across months.
         "monthly" = {
           groups <- tibble::tibble(date = names(r)) %>%
             dplyr::mutate(mon = lubridate::month(date)) %$%
             mon
           
           terra::tapp(r, groups, fun = agg_func)
         })
}


aggregate_gridmet <-
  function(base_dir, variable, period) {
    agg_func = ifelse(variable == 'pr', 'sum', 'mean')
    stopifnot("period must be one of 'annual' or 'monthly'" = period %in% c("annual", "monthly"))
    
    data_dir = file.path(base_dir, variable)
    out_dir <-  file.path(data_dir, "summarized", period)
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE)
    }
    
    list.files(data_dir, full.names = T, pattern = ".tif") %>%
      grep(".aux.xml", ., invert = TRUE, value = TRUE) %>%
      lapply(function(x) {
        out_name <- glue::glue(
          "{tools::file_path_sans_ext(basename(x))}_{period}_{agg_func}.tif"
        ) %>%
          file.path(out_dir, .)
        
        print(glue::glue("Writing {out_name} ..."))
        x %>%
          terra::rast() %>%
          `names<-`(name_to_date(., variable = variable)) %>%
          subset_period(period = period, agg_func = agg_func) %>%
          terra::writeRaster(out_name, overwrite = TRUE)
      })
    
    return()
  }

