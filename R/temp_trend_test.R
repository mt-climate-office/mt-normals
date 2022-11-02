# library(magrittr)
#
# shp <- urbnmapr::get_urbn_map(sf = T) %>%
#   dplyr::filter(state_abbv == "MT") %>%
#   sf::st_transform(crs = 4326)
#
#
# rs <- list.files("~/data/gridmet/raw/tmmx/", full.names = T) %>%
#   purrr::map(function(x) {
#     print(x)
#     terra::rast(x) %>%
#       terra::crop(terra::vect(shp)) %>%
#       terra::app(fun="mean")
# })
#
# terra::rast(rs) %>%
#   {. - 273.15} -> rasts]
#
# names(rasts) <- 1979:2020
#
# test <- spat_summary(rasts, shp, "state_abbv", "year", "mean") %>%
#   dplyr::mutate(year = as.numeric(year))
#
# ggplot(test, aes(x=year, y=value)) + geom_line()
#
#
# rs <- list.files("~/data/nclimgrid/raw", full.names = T, pattern = "tmax") %>%
#   purrr::map(function(x) {
#     print(x)
#     terra::rast(x) %>%
#       terra::crop(terra::vect(shp)) %>%
#       terra::app(fun="mean")
#   })
#
# terra::rast(rs) %>%
#   {. - 273.15} -> rasts]
#
# names(rasts) <- 1979:2020
#
# test <- spat_summary(rasts, shp, "state_abbv", "year", "mean") %>%
#   dplyr::mutate(year = as.numeric(year))
#
# ggplot(test, aes(x=year, y=value)) + geom_line()
#
#
# r <- terra::rast("~/Downloads/nclimgrid_tavg.nc") %>%
#   terra::crop(terra::vect(shp)) %>%
#   terra::tapp(index = "years", fun="mean")
#
# test <- spat_summary(r, shp, "state_abbv", "year", "mean") %>%
#   dplyr::mutate(year = stringr::str_replace(year, "X", "") %>% as.numeric())
#
# ggplot(test %>% dplyr::filter(year %in% 1979:2020), aes(x=year, y=value)) + geom_line()
#
