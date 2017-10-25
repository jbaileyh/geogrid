read_polygons <- function(file)
{

  # check resulting file is polgyons
  shape <- st_read(file)
  shape <- as(shape, "Spatial")

  if (class(shape) != "SpatialPolygonsDataFrame")
  {

    stop("Please ensure you are using polygons")
  }

  shape@proj4string <- CRS(as.character(NA))

  return(shape)
}
