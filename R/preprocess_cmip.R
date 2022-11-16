filter_years <- function(r, start, end) {
  years <- which(names(r) %in% paste0("X", start:end))
  if (length(years) == 0) {
    return(NA)
  }
  terra::subset(r, years) %>%
    terra::app(fun="mean")
}

ssp_colors <- c("ssp126"="#7570b3", "ssp245"="#1b9e77", "ssp370"="#e7298a", "ssp585"="#d95f02")

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

clean_factor_period <- function(x) {
  x %>%
    stringr::str_replace("mid_", "Mid ") %>%
    stringr::str_replace("end_", "End of ") %>%
    tools::toTitleCase() %>%
    factor(levels = c("Mid Century", "End of Century"))
}

#' make_boxplot_data
#'
#' @param files A list of CMIP6 climate projection files.
#' @param pattern A string pattern to use to subset the above CMIP6 files.
#' @param shp An `sf` object that will be used to summarize the CMIP6 data.
#' @param attr_id The column in `shp` that will be used to aggregate by
#' @param fun A function used to aggregate monthly CMIP6 data to a seasonal or annual timescale.
#' @param idx Argument passed to the `indices` argument of [[terra::tapp()]].
#' @param tsfm A function to apply to the data (e.g., a function that converts from
#' degrees C to degrees F).
#'
#' @return A `tibble` of annual changes in a variable according to mid and end of
#'century projections, aggregated by `attr_id` columns.
#' @export
#'
#' @examples
#' \dontrun{
#' 1+1
#' }
make_boxplot_data <- function(files, pattern, shp, attr_id, fun, idx="years", tsfm = NULL) {

  read_and_tapp(files = files, pattern = pattern, fun = fun, idx = idx, tsfm = tsfm)  %>%
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
      diff = list(round(value - historical)),
      model=model
    )  %>%
    dplyr::mutate(diff = list(normals::spat_summary(diff, shp, attr_id, fun="mean"))) %>%
    tidyr::unnest(diff) %>%
    dplyr::select(scenario, period, area=dplyr::all_of(attr_id), diff=value) %>%
    dplyr::mutate(
      period = clean_factor_period(period)
    )
}

#' make_boxplot_plot
#'
#' @param dat A `tibble` returned by [[make_boxplot_data()]].
#' @param ylab The label to apply to the y axis of the plot.
#' @param title The title to give the figure.
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @examples
#' \dontrun{
#' 1+1
#' }
make_boxplot_plot <- function(dat, ylab, title) {

  ggplot2::ggplot(dat, ggplot2::aes(x=scenario, y=diff, fill=scenario)) +
    ggplot2::geom_violin(alpha=0.8) +
    ggplot2::facet_grid(rows = dplyr::vars(period), cols=dplyr::vars(area)) +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(values = ssp_colors) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5),
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::labs(y=ylab, x = "", fill = "Scenario", title = title) +
    ggplot2::geom_hline(yintercept = 0)
}

#' make_map_data
#'
#' @param files A list of CMIP6 climate projection files.
#' @param pattern A string pattern to use to subset the above CMIP6 files.
#' @param fun A function used to aggregate monthly CMIP6 data to a seasonal or annual timescale.
#' @param shp An `sf` object that will be used to summarise the CMIP6 data.
#' @param attr_id The column in `shp` that will be used to aggregate by
#' @param tsfm A function to apply to the data (e.g., a function that converts from
#' degrees C to degrees F).
#' @param proj A WKT string or [[sf::st_crs()]] object giving a projection to use
#' for the output data.
#'
#' @return An `sf` object that will be used to plot a map of projected mid and
#'  end of century changes in a given climate variable
#' @export
#'
#' @examples
#' \dontrun{
#' 1+1
#' }
make_map_data <- function(files, pattern, fun, shp, attr_id, tsfm = NULL, proj=normals::mt_state_plane) {
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
      diff = list(round(value - historical)),
      model=model
    ) %>%
    dplyr::group_by(scenario, period) %>%
    dplyr::summarise(
      diff = list(terra::app(terra::rast(diff), fun="mean"))
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      diff = list(to_shp(diff, shp=shp, proj=proj))
    ) %>%
    dplyr::select(scenario, period, diff) %>%
    tidyr::unnest(diff) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(
      period = clean_factor_period(period)
    )

}

#' make_map_plot
#'
#' @param dat A `tibble` returned by [[make_map_data()]].
#' @param hot A boolean specifying whether to use a hot or cool colorscale.
#' Defaults to TRUE.
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @examples
#' \dontrun{
#' 1+1
#' }
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

  fig <- ggplot2::ggplot(dat) +
    ggplot2::geom_sf(ggplot2::aes(fill=value), color=NA) +
    ggplot2::geom_sf(ggplot2::aes(), data = climdiv, fill=NA) +
    ggplot2::facet_grid(rows = dplyr::vars(scenario), cols = dplyr::vars(period)) +
    ggplot2::scale_fill_gradient2(
      low = pal[1], mid = pal[2], high =  pal[3],
      midpoint = midpoint
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5),
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::labs(y="", x = "", fill = "")

  return(fig)
}

#' apply_map_by_scenario
#'
#' @description A workaround to using [[ggplot2::facet_wrap()]]. You can't facet_wrap
#' with [[ggplot2::geom_sf()]], so this functio  is a workaround allowing you to
#' effectively facet_wrap with a `geom_sf`.
#'
#' @param dat A `tibble` returned by [[make_map_data()]].
#' @param hot A boolean specifying whether to use a hot or cool colorscale.
#' Defaults to TRUE.
#' @param title_txt A string to use as the plot title.
#'
#'
#' @return A `cowplot` object.
#' @export
#'
#' @examples
#' \dontrun{
#' 1+1
#' }
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
   ggplot2::theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = ggplot2::margin(0, 0, 0, 0)
    )
  cowplot::plot_grid(
    title, plt,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )
}

#' make_heatmap_data
#'
#' @param files A list of CMIP6 climate projection files.
#' @param pattern A string pattern to use to subset the above CMIP6 files.
#' @param fun A function used to aggregate monthly CMIP6 data to a seasonal or annual timescale.
#' @param shp An `sf` object that will be used to summarise the CMIP6 data.
#' @param attr_id The column in `shp` that will be used to aggregate by
#' @param tsfm A function to apply to the data (e.g., a function that converts from
#' degrees C to degrees F).
#'
#' @return An `tibble` object that will be used to plot a heatmap of projected mid and
#'  end of century changes in a given climate variable
#' @export
#'
#' @examples
#' \dontrun{
#' 1+1
#' }
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

#' make_heatmap_plot
#'
#' @param dat A `tibble` returned by [[make_heatmap_data()]].
#' @param hot A boolean specifying whether to use a hot or cool colorscale.
#' Defaults to TRUE.
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @examples
#' \dontrun{
#' 1+1
#' }
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

  ggplot2::ggplot(dat) +
    ggplot2::geom_tile(ggplot2::aes(x=month, y=area, fill=diff), color="black") +
    ggplot2::scale_fill_gradient2(
      low = pal[1], mid = pal[2], high =  pal[3],
      midpoint = midpoint
    ) +
    ggplot2::facet_grid(rows = dplyr::vars(scenario), cols = dplyr::vars(period)) +
    ggplot2::theme_bw() +
    ggplot2::labs(x="", y = "", fill = "") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5))
}

#' apply_map_by_scenario
#'
#' @description A workaround to using [[ggplot2::facet_wrap()]]. Using this function
#' gives plots with unique legends for easier interpretation.
#'
#' @param dat A `tibble` returned by [[make_heatmap_data()]].
#' @param hot A boolean specifying whether to use a hot or cool colorscale.
#' Defaults to TRUE.
#' @param title_txt A string to use as the plot title.
#'
#'
#' @return A `cowplot` object.
#' @export
#'
#' @examples
#' \dontrun{
#' 1+1
#' }
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
    ggplot2::theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = ggplot2::margin(0, 0, 0, 0)
    )
  cowplot::plot_grid(
    title, plt,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )
}
