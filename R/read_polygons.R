#' Import spatial data.
#'
#' Simple function to read spatial data into a SpatialPolygonsDataFrame. Based on st_read from package sf.
#' @param file A file path pointing to a shapefile or GeoJSON file, or a character string holding GeoJSON data. See the \code{dsn} argument of \code{\link[sf]{st_read}} for more details.
#' @importFrom sf st_read
#' @importFrom sp CRS
#' @export
read_polygons <- function(file) {
  .Deprecated("sf::st_read")

  # check resulting file is polgyons
  shape <- sf::st_read(file)
  shape <- methods::as(shape, "Spatial")

  if (class(shape) != "SpatialPolygonsDataFrame")
    stop("Please ensure you are using polygons")

  shape@proj4string <- sp::CRS(as.character(NA))

  return(shape)
}
