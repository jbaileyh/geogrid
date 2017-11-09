get_shape_details <- function(input_shape)
{
  input_shape <- input_shape
  # function to get shape details
  nhex <- length(input_shape)

  if (nhex < 4)
  {
    stop("Your shape has fewer than 5 polygons. Please be aware that a hexmap may have limited value.")
  }

  # Start off with guidance but start with bins that are too large
  # (cellsize too large)
  shape_summary <- input_shape@bbox
  
  #xmax <- shape_summary[2][[1]][1, 2]
  xmax <- input_shape@bbox[3]
  #ymax <- shape_summary[2][[1]][2, 2]
  ymax <- input_shape@bbox[4]
  #xmin <- shape_summary[2][[1]][1, 1]
  xmin <- input_shape@bbox[1]
  #ymin <- shape_summary[2][[1]][2, 1]
  ymin <- input_shape@bbox[2]
  xrange <- (xmax - xmin)
  yrange <- (ymax - ymin)
  start_width <- ifelse(xrange > yrange, xrange, yrange)

  # Let's assume that the user want's something more than 4 hexagons wide
  # or long. If they want something this small then a hexmap is probably
  # not worth it.
  start_size <- start_width/100
  total_area <- input_shape@polygons[[1]]@Polygons[[1]]@area


  shape_details <- list(nhex, xmax, ymax, xmin, ymin, xrange, yrange,
                        start_size, total_area)
  names(shape_details) <- c("nhex", "xmax", "ymax", "xmin", "ymin", "xrange",
                            "yrange", "start_size", "total_area")

  return(shape_details)

}
