library(magrittr)
library(ggplot2)

base_url = "https://data.climate.umt.edu/mt-normals/cog"



stat_map <- function(e) {
  switch(
    e,
    'alpha'="Alpha Parameter",
    'beta'="Beta Parameter",
    'mean'="Mean",
    'median'='Median',
    'mode'='Mode',
    'variance'='Variance'
  )
}
color_map <- function(e) {
  switch(
    e, 
    'pr' = 'BrBG',
    'tmmn'='Blues',
    'tmmx'='Reds',
    'rmax'='BrBG',
    'rmin'='BrBG',
    'th'='PuRd',
    'erc'='PuRd',
    'vpd'='PuRd',
    'vs'='PuRd',
    'sph'='PuOr',
    'srad'='RdBu'
  )
}

time_map <- function(e) {
  switch(
    e,
    'annual'='Annual',
    'jan'='January',
    'feb'='February',
    'mar'='March',
    'apr'='April',
    'may'='May',
    'jun'='June',
    'jul'='July',
    'aug'='August',
    'sep'='September',
    'oct'='October',
    'nov'='November',
    'dec'='December'
  )
}

legend_map <- function(e) {
  switch(
    e,
    'rmax'='Relative\nHumidity (%)',
    'rmin'='Relative\nHumidity (%)',
    'sph'='Specific\nHumidity (kg/kg)',
    'th'='Wind\nDirection (deg.)',
    'srad'='Solar\nRadiation (W/m^2)',
    'vs'='Wind\nSpeed (m/s)',
    'erc'='Energy Release\nIndex',
    'vpd'='Vapor Pressure\nDeficit (kPa)', 
    'pr'='Precipitation (in)',
    'tmmx'='Temperature (F)',
    'tmmn'='Temperature (F)',
    'alpha'='Alpha Parameter',
    'beta'='Beta Parameter',
  )
}

name_map <- function(e) {
  switch(
    e,
    'erc'='Energy Release Component',
    'pr'='Precipitation',
    'rmax'='Max. Relative Humidity',
    'rmin'='Min. Relative Humidity',
    'sph'='Specific Humidity',
    'srad'='Solar Radiation',
    'tmmn'='Min. Air Temperature',
    'tmmx'='Max. Air Temperature',
    'vpd'='Vapor Pressure Deficit',
    'vs'='Wind Speed'
  )
}

statistic_map <- function(e) {
  switch(
    e, 
    'alpha'='PuOr',
    'beta'='YlOrRd'
  )
}

get_df <- function(x) {
  out <- cbind(raster::xyFromCell(x, seq_len(raster::ncell(x))),
               tibble::tibble(ID = raster::getValues(x))) %>%
    tibble::as_tibble()
  
  if (is.factor(x)) {
    levels <- levels(x)[[1]] %>%
      dplyr::mutate_all(.funs = list(ordered)) %>%
      tibble::as_tibble()
    
    fact <- out$ID %>%
      ordered(levels = levels(levels$ID))
    
    out %<>%
      dplyr::mutate(ID = fact) %>%
      dplyr::left_join(levels)
  }
  
  return(out)
}


to_shp <- function(x, v, s) {

    if (v == 'pr' & !(s %in% c("alpha", "beta"))) {
      x = x/25.4
    }  else if (v == 'tmmx' & !(s %in% c("alpha", "beta"))) {
      x = (x - 273.15) * (9/5) + 32 
    } else if (v == 'tmmn' & !(s %in% c("alpha", "beta"))) {
      x = (x - 273.15) * (9/5) + 32 
    } else {
      x = x
    }
  
  x %>%
    terra::project(
      mcor::mt_state_plane %>% as.character() %>% magrittr::extract(2)
    ) %>% 
    terra::mask(mcor::mt_state %>% terra::vect(), touches=T) %>% 
    raster::raster() %>% 
    mcor::mco_mask(mask = mcor::mt_state) %>% 
    spex::qm_rasterToPolygons(na.rm = T) 
}

add_mt_background <- function() {
  # Plot the county boundaries
  ggplot2::geom_sf(data = mcor::mt_state_simple,
                   fill = "white",
                   color = "transparent")
}


add_counties <- function() {
  # Plot the county boundaries
  ggplot2::geom_sf(
    data = mcor::mt_counties_simple,
    fill = NA,
    color = "white",
    size = 0.1
  )
}


add_hillshade <- function() {
  # Plot the hillshade using the "alpha hack"
  list(
    ggplot2::geom_raster(
      data = mcor::mt_hillshade_500m %>%
        get_df(),
      mapping = aes(x = x,
                    y = y,
                    alpha = ID),
      na.rm = TRUE
    ),
    scale_alpha(
      range = c(0.8, 0),
      na.value = 0,
      limits = c(0, 255),
      guide = "none"
    )
  )
}

mtd_plot <- function(legend = TRUE){
  list(
    add_hillshade(),
    add_counties(),
    ggplot2::labs(x = NULL, y = NULL),
    mcor::mco_theme_map()
  )
}

plot_map <- function(f_url, variable, statistic, time, out_dir) {
  
  direction = ifelse(variable %in% c('srad', 'tmmx'), 1, -1)
  direction = ifelse(statistic %in% c('alpha', 'beta'), 1, direction)
  
  pal <- ifelse(
    statistic %in% c('alpha', 'beta'), 
    statistic_map(statistic), 
    color_map(variable)
  )
  
  legend_title <- ifelse(
    statistic %in% c('alpha', 'beta'), 
    legend_map(statistic),
    legend_map(variable)
  )
  
  plot_title <- glue::glue(
    "{time_map(time)} {name_map(variable)} {stat_map(statistic)} (1991-2020)"
  )
  
  dat <- terra::rast(f_url) %>% 
    to_shp(variable, statistic)
  
  p <- ggplot() + 
    geom_sf(
      data = dat, 
      mapping = aes(fill = !!rlang::sym(statistic)),
      color = "transparent"
    ) + 
    scale_fill_distiller(
      name=legend_title,
      palette = pal, 
      direction = direction
    ) + 
    mtd_plot() + 
    geom_sf(
      data = mcor::mt_state_simple,
      fill = NA,
      color = 'black',
      size = 1
    ) + 
    labs(title = plot_title) +
    theme(plot.title = element_text(hjust=0.1, colour = "gray15", face = "bold", size=10),
          legend.title =  element_text(hjust= 0.5, colour="gray15", face = "bold",
                                       size = 7),
          legend.text =   element_text(colour="gray26", face = "bold", size = 6),

    )
  
  out <- cowplot::ggdraw() + 
    cowplot::draw_plot(p) + 
    cowplot::draw_image(
      magick::image_read_svg("~/git/mt-normals/assets/MCO_logo.svg"),
      x = 0.925, y = 0.935, hjust = 1, vjust = 1, halign = 1, valign = 1, width = 0.2
    ) + 
    cowplot::draw_label("Montana Forest & Conservation Experiment Station\nUniversity of Montana\n32 Campus Drive\nMissoula, MT 59812\nPhone: (406) 243-6793\nstate.climatologist@umontana.edu",
               color = "gray26", size = 7, x=0.7, y=0.125)
  
  out_name <- file.path(out_dir, variable, glue::glue("{time}_{statistic}.png"))
  cowplot::save_plot(out_name, out, base_width = 6.25, base_height=4.67)
  return(out_name)
}

dat <- tidyr::crossing(
  element = c("erc", "pr", "rmax", "rmin", "sph", "srad", "tmmn", "tmmx", "vpd", "vs"),
  time = c("annual", tolower(month.abb)),
  statistic = c("alpha", "beta", "mean", "median", "mode", "variance")  
) %>% 
  dplyr::mutate(
    f_url = glue::glue("{base_url}/{element}/{time}_{statistic}.tif"),
  ) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    out = plot_map(f_url, element, statistic, time, "~/data/gridmet/processed/montana/normals/maps")
  )
