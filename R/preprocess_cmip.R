filter_years <- function(r, start, end) {
  years <- which(names(r) %in% paste0("X", start:end))
  if (length(years) == 0) {
    return(NA)
  }
  terra::subset(r, years) %>%
    terra::app(fun="mean")
}

ssp_colors <- c("SSP1-2.6"="#7570b3", "SSP2-4.5"="#1b9e77", "SSP3-7.0"="#e7298a", "SSP5-8.5"="#d95f02")

tas_tsfm <- function(x) (x - 273.15) * 1.8 + 32
pr_tsfm <- function(x) x/25.4

calc_agreement <- function(x) {
  x %<>% round(5)

  above <- sum(x >= 0)
  below <- sum(x < 0)

  if (above > below) {
    num <- (above / length(x)) * 100
    txt <- "increase"
  } else if (above < below) {
    num <- (below / length(x)) * 100
    txt <- "decrease"
  } else if (above == below) {
    num <- 50
    txt <- "no change"
  }

  return(
    tibble::tibble(
      "agree"=num,
      "txt"=txt
    )
  )
}

summarize_yearmonths <- function(r, start, end, is.annual=FALSE, fun = "mean", idx = "months") {

  if (is.annual) {
    yearmons <- tidyr::crossing(
      a="X", b=start:end, c="01"
    ) %>%
      dplyr::mutate(out = glue::glue("{a}{b}{c}")) %$%
      as.character(out)

    r_names <- "Annual"
  } else {
    yearmons <- tidyr::crossing(
      a="X", b=start:end, c=stringr::str_pad(1:12, 2, "left", "0")
    ) %>%
      dplyr::mutate(out = glue::glue("{a}{b}{c}")) %$%
      as.character(out)

    r_names <- month.name
  }

  yearmons_idx <- which(names(r) %in% yearmons)
  if (length(yearmons_idx) == 0) {
    return(NA)
  }

  r <- terra::subset(r, yearmons_idx)
  terra::time(r) <- as.Date(paste0(yearmons, "01"), format = "X%Y%m%d")
  r <- terra::tapp(r, fun=fun, index = idx, na.rm  = T)

  if (idx != "years") {
    names(r) <- r_names
  }
  return(r)
}

read_and_tapp <- function(pattern, files, fun, idx, tsfm=NULL) {
  out <- grep(pattern, files, value = T) %>%
    purrr::map(function(x) {

      r <- read_from_server(x) %>%
        terra::tapp(index = idx, fun = fun)
      tibble::tibble(
        f = tools::file_path_sans_ext(x) %>% basename(),
        r = list(r)
      )
    }) %>%
    dplyr::bind_rows() %>%
    tidyr::separate(f, c("model", "scenario", "run", "variable"), sep = "_") %>%
    tidyr::pivot_wider(names_from=scenario, values_from = r) %>%
    tidyr::pivot_longer(dplyr::starts_with("ssp"), names_to = "scenario") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(r = list(c(historical, value))) %>%
    dplyr::select(model, run, variable, scenario, r)

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


clean_factor_scenario <- function(x, short=FALSE) {

  if (short) {
    out <- forcats::as_factor(x) %>%
      forcats::fct_recode(
        "SSP1-2.6" = "ssp126",
        "SSP2-4.5" = "ssp245",
        "SSP3-7.0" = "ssp370",
        "SSP5-8.5" = "ssp585",
      )
  } else {
    out <- forcats::as_factor(x) %>%
      forcats::fct_recode(
        "Moderating Emissions\n(SSP1-2.6)" = "ssp126",
        "Middle of the Road\n(SSP2-4.5)" = "ssp245",
        "High Emissions\n(SSP3-7.0)" = "ssp370",
        "Accelerating Emissions\n(SSP5-8.5)" = "ssp585",
      )
  }

  return(out)
}

#' make_boxplot_data
#'
#' @param f Preprocessed cmip6 data returned by [[cmip_monthly_to_change]].
#' @param shp An `sf` object that will be used to summarize the CMIP6 data.
#' @param attr_id The column in `shp` that will be used to aggregate by
#'
#' @return A `tibble` of annual changes in a variable according to mid and end of
#'century projections, aggregated by `attr_id` columns.
#' @export
#'
#' @examples
#' \dontrun{
#' 1+1
#' }
make_boxplot_data <- function(f, shp, attr_id, fun="mean") {

  readRDS(f) %>%
    dplyr::mutate(diff = list(terra::rast(diff))) %>%
    dplyr::mutate(diff = list(terra::app(diff, fun=fun))) %>%
    dplyr::mutate(diff = list(spat_summary(diff, shp, attr_id, fun="mean"))) %>%
    tidyr::unnest(diff) %>%
    dplyr::select(scenario, model, period, area=dplyr::all_of(attr_id), diff=value) %>%
    dplyr::mutate(
      period = clean_factor_period(period),
      scenario = clean_factor_scenario(scenario, short = TRUE)
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
#' @param f The preprocessed .Rds file to read from.
#' @param shp The [[sf]] object to use to aggregate data by.
#' @param attr_id The column name in `shp` to aggregate data by.
#' @param proj A WKT string or [[sf::st_crs()]] object giving a projection to use
#' for the output data.
#' @param attr_id The column in `shp` that will be used to aggregate by
#' @param fun A function to aggreagte each region in `shp` by.
#'
#' @return An `sf` object that will be used to plot a map of projected mid and
#'  end of century changes in a given climate variable
#' @export
#'
#' @examples
#' \dontrun{
#' 1+1
#' }
make_map_data <- function(f, shp, attr_id, proj=mt_state_plane, fun="mean") {

  readRDS(f) %>%
    dplyr::mutate(diff = list(terra::rast(diff))) %>%
    dplyr::mutate(diff = list(terra::app(diff, fun = fun))) %>%
    dplyr::group_by(scenario, period) %>%
    dplyr::summarise(
      diff = list(terra::app(terra::rast(diff), fun="mean"))
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      diff = list(spat_summary(diff, shp, attr_id, fun = "mean"))
    ) %>%
    tidyr::unnest(diff) %>%
    sf::st_as_sf() %>%
    dplyr::select(scenario, period, value) %>%
    sf::st_as_sf() %>%
    sf::st_transform(proj) %>%
    dplyr::mutate(
      period = clean_factor_period(period),
      scenario = clean_factor_scenario(scenario)
    )
}

#' make_map_plot
#'
#' @param dat A `tibble` returned by [[make_map_data()]].
#' @param shp The [[sf]] object to use as an outline of the map.
#' @param title_txt The title to use for the map.
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
make_map_plot <- function(dat, shp, title_txt, hot=TRUE) {

  diverging <- any(dat$value < 0)
  midpoint <- ifelse(diverging, 0, mean(dat$value))

  if (hot && diverging) {
    pal <-  c('#fc8d59','#ffffbf','#91bfdb')
    pal <- ggplot2::scale_fill_gradient2(
      low = pal[1], mid = pal[2], high =  pal[3],
      midpoint = midpoint
    )
  } else if (hot && !diverging) {
    pal <- viridis::scale_fill_viridis(option = "magma")
  } else if (!hot && diverging) {
    pal <- c('#d8b365','#f5f5f5','#5ab4ac')
    pal <- ggplot2::scale_fill_gradient2(
      low = pal[1], mid = pal[2], high =  pal[3],
      midpoint = midpoint
    )
  } else {
    pal <- viridis::scale_fill_viridis(option = "viridis")
  }

  ggplot2::ggplot(dat) +
    ggplot2::geom_sf(ggplot2::aes(fill=value), color="white") +
    ggplot2::geom_sf(data = shp, mapping = ggplot2::aes(), fill = NA, color="black") +
    ggplot2::facet_grid(rows = dplyr::vars(scenario), cols = dplyr::vars(period)) +
    pal +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.key.height = unit(0.10, "npc")
    ) +
    ggplot2::labs(y="", x = "", fill = "", title = title_txt)

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
apply_map_by_scenario <- function(dat, shp, hot, title_txt) {
  plt <- dat %>%
    dplyr::group_by(scenario) %>%
    dplyr::group_split() %>%
    lapply(make_map_plot, shp = shp, hot = hot) %>%
    cowplot::plot_grid(plotlist=., nrow=length(.))

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
#' @param f An `.rds` file created by [[cmip_monthly_to_change]].
#' @param shp An `sf` object that will be used to summarise the CMIP6 data.
#' @param attr_id The column in `shp` that will be used to aggregate by, defaults to NULL.
#' If left blank, results will be aggregated across the entire domain.
#'
#' @return An `tibble` object that will be used to plot a heatmap of projected mid and
#'  end of century changes in a given climate variable
#' @export
#'
#' @examples
#' \dontrun{
#' 1+1
#' }
make_heatmap_data <- function(f, shp, attr_id=NULL) {

  if (is.null(attr_id)) {
    attr_id = "domain"
    shp %<>% dplyr::mutate(domain = "domain")
  }

  readRDS(f) %>%
    dplyr::mutate(diff = list(terra::rast(diff))) %>%
    dplyr::group_by(scenario, period, model) %>%
    dplyr::summarise(
      diff = list(terra::tapp(terra::rast(diff), index = month.name, fun = "mean")),
      .groups = "drop"
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      diff = list(spat_summary(diff, shp, attr_id, "month", "mean"))
    ) %>%
    tidyr::unnest(diff) %>%
    dplyr::select(scenario, model, period, area=dplyr::all_of(attr_id), month, diff=value) %>%
    dplyr::mutate(
      period = clean_factor_period(period),
      month = factor(month, levels=month.name)
    ) %>%
    dplyr::group_by(scenario, period, month, area) %>%
    dplyr::summarise(
      agree = calc_agreement(diff),
      diff = mean(diff),
    ) %>%
    tidyr::unnest(cols = agree) %>%
    dplyr::mutate(scenario = clean_factor_scenario(scenario))
}

#' make_heatmap_plot
#'
#' @param dat A `tibble` returned by [[make_heatmap_data()]].
#' @param hot A boolean specifying whether to use a hot or cool colorscale.
#' Defaults to TRUE.
#' @param title_txt The title to give to the plot.
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @examples
#' \dontrun{
#' 1+1
#' }
make_heatmap_plot <- function(dat, hot = TRUE, title_txt) {

  diverging <- any(dat$diff < 0)
  midpoint <- ifelse(diverging, 0, mean(dat$diff))

  if (hot && diverging) {
    pal <-  c('#fc8d59','#ffffbf','#91bfdb')
    pal <- ggplot2::scale_fill_gradient2(
      low = pal[1], mid = pal[2], high =  pal[3],
      midpoint = midpoint
    )
  } else if (hot && !diverging) {
    pal <- viridis::scale_fill_viridis(option = "magma")
  } else if (!hot && diverging) {
    pal <- c('#d8b365','#f5f5f5','#5ab4ac')
    pal <- ggplot2::scale_fill_gradient2(
      low = pal[1], mid = pal[2], high =  pal[3],
      midpoint = midpoint
    )
  } else {
    pal <- viridis::scale_fill_viridis(option = "viridis")
  }

  ggplot2::ggplot(dat) +
    ggplot2::geom_tile(ggplot2::aes(x=month, y=area, fill=diff), color="black") +
    pal +
    ggplot2::facet_grid(rows = dplyr::vars(scenario), cols = dplyr::vars(period)) +
    ggplot2::theme_bw() +
    ggplot2::labs(x="", y = "", fill = "", title = title_txt) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5),
      legend.key.height = unit(0.10, "npc")
    )
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
    cowplot::plot_grid(plotlist = ., nrow = length(.))

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

#' cmip_monthly_to_change
#'
#' @param files list of cmip6 files to preprocess.
#' @param pattern The pattern to use to filter `files`
#' @param fun The function to use to aggregate rasters with (usually either
#' "mean" or "sum").
#' @param tsfm A function to apply to the rasters to convert units (optional).
#' @param is.annual Boolean of whether the CMIP files are already aggregated to a annual timescale.
#' @param out_dir The directory to save the files out to.
#'
#' @return
#' @export
#'
#' @examples
cmip_monthly_to_change <- function(files, pattern, fun, tsfm, is.annual, out_dir) {

  out_name = file.path(
    out_dir,
    glue::glue("cmip_{stringr::str_replace(pattern, '.tif', '.rds')}")
  )
  print(glue::glue("{out_name}: {is.annual}"))
  read_and_tapp(
    files = files, pattern = pattern, fun = fun, idx = "yearmonths", tsfm = tsfm
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      hist = list(summarize_yearmonths(r, 1991, 2020, is.annual)),
      mid_century = list(summarize_yearmonths(r, 2040, 2069, is.annual)),
      end_century = list(summarize_yearmonths(r, 2070, 2099, is.annual))
    ) %>%
    dplyr::select(model, scenario, hist, mid_century, end_century) %>%
    tidyr::pivot_longer(dplyr::ends_with("century"), names_to = "period") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(diff = list(terra::wrap(value - hist))) %>%
    dplyr::select(model, scenario, period, diff) %>%
    saveRDS(out_name)

  return(out_name)
}

#' cmip_ppt_interannual_variability
#'
#' @description Calculate interannual precipitation variability
#'
#' @param files List of CMIP6 files to be filtered.
#' @param out_dir The directory to save the results out to.
#'
#' @return Save out an `rds` file of interannual variability
#' @export
#'
#' @examples
#' \notrun{
#' 1+1
#' }
cmip_ppt_interannual_variability <- function(files, out_dir) {

  out_name = file.path(
    out_dir, "cmip_iv.rds"
  )
  read_and_tapp(
    files = files, pattern = "pr.tif", fun = "sum", idx = "yearmonths", tsfm = pr_tsfm
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      hist = list(summarize_yearmonths(r, 1991, 2020, FALSE, fun = "sd", idx = "years")),
      mid_century = list(summarize_yearmonths(r, 2040, 2069, FALSE, fun = "sd", idx = "years")),
      end_century = list(summarize_yearmonths(r, 2070, 2099, FALSE, fun = "sd", idx = "years"))
    ) %>%
    dplyr::select(model, scenario, hist, mid_century, end_century) %>%
    tidyr::pivot_longer(dplyr::ends_with("century"), names_to = "period") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(diff = list(terra::wrap(value - hist))) %>%
    dplyr::select(model, scenario, period, diff) %>%
    saveRDS(out_name)

}

# cmip_files <- list.files("~/data/cmip/monthly", full.names = T) %>%
#   grep(".json", ., value = T, invert = T)
#
# tibble::tribble(
#   ~pattern, ~title, ~tsfm, ~hot, ~fun, ~is.annual,
#   "above90.tif", "Change in # of Days Above 90°F", NULL, TRUE, "sum", FALSE,
#   "con-dry.tif", "Change in # of Consecutive Dry Days", NULL, FALSE, "sum", TRUE,
#   "con-wet.tif", "Change in # of Consecutive Wet Days", NULL, FALSE, "sum", TRUE,
#   "dry-days.tif", "Change in # of Dry Days", NULL, FALSE, "sum", FALSE,
#   "wet-days.tif", "Change in # of Wet Days", NULL, FALSE, "sum", FALSE,
#   "freeze-free.tif", "Change in # of Freeze-Free Days", NULL, TRUE, "sum", FALSE,
#   "pr.tif", "Change in Annual Total Precipitation (in.)", pr_tsfm, FALSE, "sum", FALSE,
#   "tas.tif", "Change in Mean Annual Temperature (°F)", tas_tsfm, TRUE, "mean", FALSE,
#   "tasmax.tif", "Change in Mean Annual Maximum Temperature (°F)", tas_tsfm, TRUE, "mean", FALSE,
#   "tasmin.tif", "Change in Mean Annual Minimum Temperature (°F)", tas_tsfm, TRUE, "mean", FALSE
# ) %>%
#   dplyr::rowwise() %>%
#   dplyr::mutate(new_name = cmip_monthly_to_change(
#     files=cmip_files, pattern=pattern, fun=fun, tsfm=tsfm,
#     is.annual = is.annual, out_dir = "~/git/mco/MCA/assets")
#   )
