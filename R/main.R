source("./crop_to_roi.R")
source("./aggregate_period.R")
source("./normals_slim.R")


# Read in a shapefile of Montana to clip results to.
roi <- urbnmapr::get_urbn_map(sf = TRUE) %>% 
  dplyr::filter(state_abbv == 'MT') %>% 
  sf::st_transform(4326) 

# define data directories and vairables to use. 
variables = c("etr", "pet")
raw_dir = "~/data/gridmet/raw"
summary_dir = "~/data/gridmet/processed/montana"

# Crop the raw gridMET data to the Montana ROI
crop_to_roi(raw_dir, summary_dir, roi, variables)

# Aggregate variables to monthly and annual timescales. 
lapply(variables, function(x) {
  aggregate_gridmet(summary_dir, x, "monthly")
  aggregate_gridmet(summary_dir, x, "annual")
})


mask = mcor::mt_state_simple %>% sf::st_transform(4326) %>% terra::vect()

# Calculate Gamma metrics for all variables
out <- tidyr::crossing(
  variables = variables,
  periods = c("monthly", "annual")
) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(complete = list(iterate_options(variables, periods, summary_dir, mask)))

normals_dir = file.path(summary_dir, "normals")
