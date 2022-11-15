f_list <- RCurl::getURL(
  "https://data.climate.umt.edu/mca/cmip/",
  ftp.use.epsv = TRUE,
  dirlistonly = FALSE
) %>%
  XML::getHTMLLinks() %>%
  grep(".tif", ., value = T) %>%
  grep(".json", ., value = T, invert = T)

cmip_files <- file.path(
  "https://data.climate.umt.edu/mca/cmip",
  basename(f_list)
)

filter_years <- function(r, start, end) {
  years <- which(names(r) %in% paste0("X", start:end))
  if (length(years) == 0) {
    return(NA)
  }
  terra::subset(r, years) %>%
    terra::app(fun="mean")
}

summarize_yearmonths <- function(r, start, end) {

  yearmons <- tidyr::crossing(
    a="X", b=start:end, c=stringr::str_pad(1:12, 2, "left", "0")
  ) %>%
    dplyr::mutate(out = glue::glue("{a}{b}{c}")) %$%
    as.character(out)

  yearmons_idx <- which(names(r) %in% yearmons)
  if (length(yearmons_idx) == 0) {
    return(NA)
  }

  r <- terra::subset(r, yearmons_idx)
  terra::time(r) <- lubridate::as_date(paste0(yearmons, "01"), format = "X%Y%m%d")
  r <- terra::tapp(r, fun="mean", index = "months")
  names(r) <- month.name
  return(r)
}

join_historical <- function(dat) {
  dat %>%
    dplyr::select(model, scenario, hist, mid_century, end_century) %>%
    dplyr::left_join(
      dplyr::filter(., scenario == "historical") %>%
        dplyr::select(model, historical=hist),
      by = "model"
    ) %>%
    dplyr::select(-hist) %>%
    dplyr::filter(scenario != "historical") %>%
    tidyr::pivot_longer(
      c("mid_century", "end_century")
    )
}

read_and_tapp <- function(pattern, files, fun, idx, tsfm=NULL) {
  out <- grep(pattern, files, value = T) %>%
    purrr::map(function(x) {

      r <- normals::read_from_server(x) %>%
        terra::tapp(index = idx, fun = fun)
      tibble::tibble(
        f = tools::file_path_sans_ext(x) %>% basename(),
        r = list(r)
      )
    }) %>%
    dplyr::bind_rows() %>%
    tidyr::separate(f, c("model", "scenario", "run", "variable"), sep = "_")

  if (!is.null(tsfm)) {
    out <-
      dplyr::rowwise(out) %>%
      dplyr::mutate(r = list(tsfm(r))) %>%
      dplyr::ungroup()
  }
  return(out)
}

climdiv_to_factor <- function(x) {
  factor(x, levels = c("Western", "Southwestern", "North Central", "Central", "South Central", "Northeastern", "Southeastern"))
}

clean_factor_period <- function(x) {
  x %>%
    stringr::str_replace("mid_", "Mid ") %>%
    stringr::str_replace("end_", "End of ") %>%
    tools::toTitleCase() %>%
    factor(levels = c("Mid Century", "End of Century"))
}


make_boxplot_plot <- function(dat, ylab, title) {

  ggplot(dat, aes(x=scenario, y=diff, fill=scenario)) +
    geom_violin(alpha=0.8) +
    facet_grid(rows = dplyr::vars(period), cols=dplyr::vars(area)) +
    theme_bw() +
    scale_fill_manual(values = ssp_colors) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      plot.title = element_text(hjust = 0.5)
    ) +
    labs(y=ylab, x = "", fill = "Scenario", title = title) +
    geom_hline(yintercept = 0)
}

make_map_data <- function(files, pattern, fun, shp, attr_id, tsfm = NULL) {
  read_and_tapp(files = files, pattern = pattern, fun = fun, idx = "years", tsfm = tsfm)  %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      hist = list(filter_years(r, 1981, 2010)),
      mid_century = list(filter_years(r, 2040, 2069)),
      end_century = list(filter_years(r, 2070, 2099))
    ) %>%
    join_historical() %>%
    dplyr::rowwise() %>%
    dplyr::transmute(
      scenario = scenario,
      period = name,
      diff = list(value - historical),
      model=model
    ) %>%
    dplyr::group_by(scenario, period) %>%
    dplyr::summarise(
      diff = list(terra::app(terra::rast(diff), fun="mean"))
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      diff = list(to_shp(diff, mt=mt))
    ) %>%
    dplyr::select(scenario, period, diff) %>%
    tidyr::unnest(diff) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(
      period = clean_factor_period(period)
    )

}

make_map_plot <- function(dat, hot=TRUE) {

  diverging <- any(dat$value < 0)
  if (hot && diverging) {
    pal <-  c('#fc8d59','#ffffbf','#91bfdb')
  } else if (hot && !diverging) {
    pal <- c('#fee8c8','#fdbb84','#e34a33')
  } else if (!hot && diverging) {
    pal <- c('#d8b365','#f5f5f5','#5ab4ac')
  } else {
    pal <- c('#ece7f2','#a6bddb','#2b8cbe')
  }

  midpoint <- ifelse(diverging, 0, mean(dat$value))

  fig <- ggplot(dat) +
    geom_sf(aes(fill=value), color=NA) +
    geom_sf(aes(), data = climdiv, fill=NA) +
    facet_grid(rows = vars(scenario), cols = vars(period)) +
    scale_fill_gradient2(
      low = pal[1], mid = pal[2], high =  pal[3],
      midpoint = midpoint
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      plot.title = element_text(hjust = 0.5)
    ) +
    labs(y="", x = "", fill = "")

  return(fig)
}

apply_map_by_scenario <- function(dat, hot, title_txt) {
  plt <- dat %>%
    dplyr::group_by(scenario) %>%
    dplyr::group_split() %>%
    lapply(make_map_plot, hot = hot) %>%
    {cowplot::plot_grid(.[[1]], .[[2]], nrow=2)}

  title <- cowplot::ggdraw() +
    cowplot::draw_label(
      title_txt,
      fontface = 'bold',
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 0)
    )
  cowplot::plot_grid(
    title, plt,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )
}

make_heatmap_data <- function(files, pattern, fun, shp, attr_id, tsfm = NULL) {

  read_and_tapp(
    files = files, pattern = pattern, fun = fun, idx = "yearmonths", tsfm = tsfm
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      hist = list(summarize_yearmonths(r, 1981, 2010)),
      mid_century = list(summarize_yearmonths(r, 2040, 2069)),
      end_century = list(summarize_yearmonths(r, 2070, 2099))
    ) %>%
    join_historical() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(diff = list(value - historical)) %>%
    dplyr::group_by(scenario, name) %>%
    dplyr::summarise(
      diff = list(terra::tapp(terra::rast(diff), index = month.name, fun = "mean")),
      .groups = "drop"
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      diff = list(normals::spat_summary(diff, shp, attr_id, "month", "mean"))
    ) %>%
    tidyr::unnest(diff) %>%
    dplyr::select(scenario, period=name, area=dplyr::all_of(attr_id), month, diff=value) %>%
    dplyr::mutate(
      period = clean_factor_period(period),
      month = factor(month, levels=month.name)
    )
}

make_heatmap_plot <- function(dat, hot = TRUE) {

  diverging <- any(dat$diff < 0)
  if (hot && diverging) {
    pal <-  c('#fc8d59','#ffffbf','#91bfdb')
  } else if (hot && !diverging) {
    pal <- c('#fee8c8','#fdbb84','#e34a33')
  } else if (!hot && diverging) {
    pal <- c('#d8b365','#f5f5f5','#5ab4ac')
  } else {
    pal <- c('#ece7f2','#a6bddb','#2b8cbe')
  }

  midpoint <- ifelse(diverging, 0, mean(dat$diff))

  ggplot(dat) +
    geom_tile(aes(x=month, y=area, fill=diff), color="black") +
    scale_fill_gradient2(
      low = pal[1], mid = pal[2], high =  pal[3],
      midpoint = midpoint
    ) +
    facet_grid(rows = vars(scenario), cols = vars(period)) +
    theme_bw() +
    labs(x="", y = "", fill = "") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
}

apply_heatmap_by_scenario <- function(dat, hot, title_txt) {
  plt <- dat %>%
    dplyr::group_by(scenario) %>%
    dplyr::group_split() %>%
    lapply(make_heatmap_plot, hot = hot) %>%
    {cowplot::plot_grid(.[[1]], .[[2]], nrow=2)}

  title <- cowplot::ggdraw() +
    cowplot::draw_label(
      title_txt,
      fontface = 'bold',
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 0)
    )
  cowplot::plot_grid(
    title, plt,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )
}

make_summary_data <- function(files, pattern, fun, shp, attr_id, tsfm = NULL) {
  read_and_tapp(files = files, pattern = pattern, fun = fun, idx = "years", tsfm = tsfm)  %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      hist = list(filter_years(r, 1981, 2010)),
      mid_century = list(filter_years(r, 2040, 2069)),
      end_century = list(filter_years(r, 2070, 2099))
    ) %>%
    join_historical() %>%
    dplyr::rowwise() %>%
    dplyr::transmute(
      scenario = scenario,
      period = name,
      diff = list(value - historical),
      model=model
    ) %>%
    dplyr::filter(scenario == "ssp585", period == "mid_century") %>%
    dplyr::mutate(
      mn = terra::mask(diff, normals::mt) %>% terra::global("min", na.rm = T),
      mx = terra::mask(diff, normals::mt) %>% terra::global("max", na.rm = T),
      avg = terra::mask(diff, normals::mt) %>% terra::global("mean", na.rm = T),
      med = terra::mask(diff, normals::mt) %>% terra::global(median, na.rm = T),
    )
}
