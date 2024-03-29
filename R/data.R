#' `sf` object of the outline of Montana.
#'
#' A dataset outlining Montana to use as an example in the repo.
#' The shapefile was taken from the `mcor` R package.
#'
#' @format A geometry set with one feature.
#' @source \url{http://ftp.geoinfo.msl.mt.gov/Data/Spatial/MSDI/AdministrativeBoundaries/MontanaCounties.zip}
"mt"

#' The Montana State Plane Coordinate Reference System
#' This object was duplicated from the [mcor](https://github.com/mt-climate-office/mcor)
#' package.
#'
#' An object of class [sf::crs][sf::st_crs()] defining the
#' *NAD 1983 HARN StatePlane Montana FIPS 2500* projection
#' coordinate system ([EPSG: 102300](https://epsg.io/102300)).
#'
#' @format An object of class [sf::crs][sf::st_crs()] of length 2.
#' * **epsg** — an integer (102300) representing the ([EPSG](http://epsg.org/)) code
#' * **proj4string** — a character string representing the associated [proj.4](http://proj4.org/) code
#'
#' @source \url{https://epsg.io/102300}
"mt_state_plane"

#' An `sf` object that splits Montana east and west of the continental divide.
#'
#' @format A geometry set with one feature.
"divide"
