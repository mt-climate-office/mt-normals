library(magrittr)

# Functions to crop gridmnet data to Montana 

roi <- urbnmapr::get_urbn_map(sf = TRUE) %>%
  dplyr::filter(state_abbv == "MT") %>%
  sf::st_transform(4326)

out_dir <- "~/data/gridmet/processed/montana"

list.files(
  "~/data/gridmet/raw/",
  recursive = T,
  full.names = T,
  pattern = ".nc"
) %>%
  lapply(function(x) {
    print(x)

    pth <- file.path(
      out_dir,
      basename(dirname(x)),
      stringr::str_replace(basename(x), ".nc", ".tif")
    )

    if (!file.exists(dirname(pth))) {
      dir.create(dirname(pth))
    }

    r <- raster::stack(x)
    c <- raster::crop(r, roi)
    raster::writeRaster(c, pth, overwrite = T)
  })
