#' Extract details from provided polygons.
#'
#' Extract spatial extent, range and other geospatial features from the output of read_polygons. Items are returned as a list for use in \code{\link{calculate_grid}}.
#'
#' @param input_shape A "SpatialPolygonsDataFrame" object representing the original spatial polygons.
get_shape_details_internal <- function(input_shape) {

  nhex <- length(input_shape)

  if (nhex < 4)
    message("Your shape has fewer than 5 polygons. ",
      "Please be aware that a geogrid may have limited value.")

  # Start off with guidance but start with bins that are too large
  # (cellsize too large)
  # shape_summary <- input_shape@bbox

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
  # or long. If they want something this small then a geogrid is probably
  # not worth it.
  start_size <- start_width / 100
  total_area <- input_shape@polygons[[1]]@Polygons[[1]]@area

  shape_details <- list(nhex = nhex, xmax = xmax, ymax = ymax,
    xmin = xmin, ymin = ymin, xrange = xrange, yrange = yrange,
    start_size = start_size, total_area = total_area)
  class(shape_details) <- c("shape_details", "list")

  return(shape_details)
}

#' Extract details from provided polygons (deprecated).
#'
#' Extract spatial extent, range and other geospatial features from the output of read_polygons. Items are returned as a list for use in \code{\link{calculate_grid}}.
#'
#' @param input_shape A "SpatialPolygonsDataFrame" object representing the original spatial polygons.
#' @export
get_shape_details <- function(input_shape) {
  stop("get_shape_details() has been deprecated. ",
    "It is now handled automatically in calculate_grid().",
    call. = FALSE)
}
