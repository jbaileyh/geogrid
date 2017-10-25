get_shape_details <- function(shape)
{

  # function to get shape details
  nhex <- length(shape)

  if (nhex < 4)
  {
    stop("Your shape has fewer than 5 polygons. Please be aware that a hexmap may have limited value.")
  }

  # Start off with guidance but start with bins that are too large
  # (cellsize too large)
  xmax <- summary(shape)[2][[1]][1, 2]
  ymax <- summary(shape)[2][[1]][2, 2]
  xmin <- summary(shape)[2][[1]][1, 1]
  ymin <- summary(shape)[2][[1]][2, 1]
  xrange <- (xmax - xmin)
  yrange <- (ymax - ymin)
  start_width <- ifelse(xrange > yrange, xrange, yrange)

  # Let's assume that the user want's something more than 4 hexagons wide
  # or long. If they want something this small then a hexmap is probably
  # not worth it.
  start_size <- start_width/4
  total_area <- shape@polygons[[1]]@Polygons[[1]]@area


  shape_details <- list(nhex, xmax, ymax, xmin, ymin, xrange, yrange,
                        start_size, total_area)
  names(shape_details) <- c("nhex", "xmax", "ymax", "xmin", "ymin", "xrange",
                            "yrange", "start_size", "total_area")

  return(shape_details)

}
