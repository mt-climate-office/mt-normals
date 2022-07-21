library(magrittr)

# Functions to crop gridmnet data to Montana 
crop_to_roi <- function(data_dir, out_dir, roi, variables) {
  
  list.files(
    data_dir,
    recursive = T,
    full.names = T,
    pattern = ".nc"
  ) %>%
    grep(paste(variables, collapse = "|"), ., value=T) %>%
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
} 



