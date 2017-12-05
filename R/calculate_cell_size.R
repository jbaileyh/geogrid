calculate_cell_size <- function(shape, shape_details, learning_rate, grid_type, seed)
{
  set.seed(seed)
  # = c('regular', 'hexagonal') check that regular and hexagon dont
  # return different lists of points (list and list[[]] respectively?)

  # Lets find some bounds for the optimisation that make sense.
  max_allowed_area <- shape_details$total_area/shape_details$nhex
  hexagon_diam <- sqrt(max_allowed_area/2.598076) * 2

  cellsize <- shape_details$start_size

  repeat {
    HexPts <- spsample(shape, type = grid_type, cellsize = cellsize, iter = 10000)
    npolygons <- length(HexPts)
    print(npolygons)
    print(cellsize)

    if (npolygons == shape_details$nhex)
      break else if (npolygons > shape_details$nhex)
      {
        print("too many polygons")
        cellsize_new <- cellsize * (1 + learning_rate)
        cellsize <- cellsize_new
      } else
      {
        # else (npolygons < shape_details$nhex)
        print("too few polygons")
        cellsize_new <- cellsize * (1 - learning_rate)
        cellsize <- cellsize_new
      }
  }

  print(paste0("The cellsize is ", cellsize))

  if (grid_type == "hexagonal")
  {
    Pols <- HexPoints2SpatialPolygons(HexPts)
  } else
  {
    Pols <- SpatialPixels(HexPts)
    Pols <- as(Pols, "SpatialPolygons")
  }
  # or spatial polygons? need to turn this into same object as hexagons
  # above try making dataframe and going that route. need correct ids for
  # match between then and now note <- cellsize could be unsolveable. Add
  # rotation of grid if needed.

  return(list(HexPts, Pols, cellsize))

}
