#' write_as_cog
#'
#' @description A wrapper around `terra::writeRaster` to write a `terra::rast`
#' to disk as a [Cloud Optimized GeoTIFF](https://www.cogeo.org/).
#'
#' @param x
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
write_as_cog <- function(x, filename) {
  terra::writeRaster(
    x = x,
    filename = filename,
    overwrite = TRUE,
    gdal = c("COMPRESS=DEFLATE",
             "of=COG"),
    memfrac = 0.9
  )
}
