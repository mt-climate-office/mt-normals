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
  function(base_dir, out_dir, variable, agg_func = "sum", pattern='.nc', years=1991:2020) {

    r <- file.path(base_dir, variable) %>%
      list.files(full.names = T, pattern = pattern) %>%
      grep(".aux.xml", ., invert = TRUE, value = TRUE) %>%
      grep(paste(years, collapse = "|"), ., value = TRUE) %>% 
      terra::rast() %>% 
      magrittr::set_names(
        stringr::word(names(.), 2, sep="=") %>% 
          as.numeric() %>%
          as.Date(origin="1900-01-01") 
      )
      
    grps <- tibble::tibble(datetime = names(r)) %>% 
      dplyr::mutate(
        month = lubridate::month(datetime),
        year = lubridate::year(datetime) - min(years)+1,
      ) %>% 
      dplyr::group_by(year, month) %>%
      dplyr::mutate(mon_grp = dplyr::cur_group_id()) %>% 
      dplyr::ungroup()
    
    monthly <- terra::tapp(r, index=grps$mon_grp, fun=agg_func, cores = 10)
    annual <- terra::tapp(r, index=grps$year, fun=agg_func)
    
      lapply(function(x) {
        out_name <- glue::glue(
          "{tools::file_path_sans_ext(basename(x))}_{period}_{agg_func}.tif"
        ) %>%
          file.path(out_dir, .)
        
        print(glue::glue("Writing {out_name} ..."))
        x %>%
          terra::rast() %>%
          `names<-`(
            stringr::word(names(.), 2, sep="=") %>% 
              as.numeric() %>%
              as.Date(origin="1900-01-01")
          ) %>%
          subset_period(period = period, agg_func = agg_func)
      })
    
    return()
  }

variables <- c("erc", "rmax", "rmin", "sph", "srad", "vpd", "vs", "tmmx", "tmmn", "pr")

for (v in variables) {

    print(glue::glue("Working on {v}..."))
    agg_func = ifelse(v == 'pr', 'sum', 'mean')
    
    test <- aggregate_gridmet(
      base_dir = "~/data/gridmet/raw/",
      out_dir = "~/data/gridmet/processed/conus",
      variable = v,
      agg_func = agg_func
    )
}