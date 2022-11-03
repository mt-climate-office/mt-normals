# library(magrittr)
# source("./crop_to_roi.R")
# source("./aggregate_period.R")
# source("./normals_slim.R")
# source("./build_min_max_txt.R")
#
#
# # Read in a shapefile of Montana to clip results to.
# roi <- mcor::mt_state_simple %>% sf::st_transform(4326)
#
# # define data directories and vairables to use.
# variables = c("erc",  "etr",  "pr",   "rmax", "rmin", "sph",  "srad", "tmmn", "tmmx", "vpd", "vs", "pet")
# variables = c("pet", "etr")
# raw_dir = "./data/raw"
# summary_dir = "~/data/gridmet/processed/montana/"
#
# # Crop the raw gridMET data to the Montana ROI
# crop_to_roi(raw_dir, summary_dir, roi, variables)
#
# # Aggregate variables to monthly and annual timescales.
# lapply(variables, function(x) {
#   aggregate_gridmet(summary_dir, x, "monthly")
#   aggregate_gridmet(summary_dir, x, "annual")
# })
#
# # Convert ROI to a terra vector object.
# mask <- roi %>% terra::vect()
#
# # Calculate and save out Gamma metrics for all variables
# out <- tidyr::crossing(
#   variables = variables,
#   periods = c("monthly", "annual")
# ) %>%
#   dplyr::rowwise() %>%
#   dplyr::mutate(complete = list(iterate_options(variables, periods, summary_dir, mask)))
#
#
# # Build a json text file for the legend information.
# normals_dir <- file.path(summary_dir, "normals")
# build_legend_txt(normals_dir)
