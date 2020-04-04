#' Assign the polygons in the original spatial data to their new location.
#'
#' Assigns each polygon in the original file to a new location in the gridded
#' geometry using the Hungarian algorithm.
#'
#' @param shape A "SpatialPolygonsDataFrame" or an sf object representing the
#' original spatial polygons.
#' @param new_polygons A "geogrid" object returned from \code{\link{calculate_grid}}.
#' @importFrom rgeos gCentroid
#' @importFrom sp SpatialPolygonsDataFrame coordinates spDistsN1 spDists merge
#' @importFrom sf st_as_sf
#' @return An object of the same class as shape
#' @export
#' @examples
#' library(sf)
#' input_file <- system.file("extdata", "london_LA.json", package = "geogrid")
#' original_shapes <- st_read(input_file) %>% st_set_crs(27700)
#'
#' # calculate grid
#' new_cells <- calculate_grid(shape = original_shapes,
#'   grid_type = "hexagonal", seed = 1)
#' grid_shapes <- assign_polygons(original_shapes, new_cells)
#' plot(grid_shapes)
#'
#' par(mfrow = c(1, 2))
#' plot(st_geometry(original_shapes))
#' plot(st_geometry(grid_shapes))
#'
#' \dontrun{
#' # look at different grids using different seeds
#' par(mfrow=c(2, 3), mar = c(0, 0, 2, 0))
#' for (i in 1:6) {
#'   new_cells <-
#'     calculate_grid(shape = original_shapes, grid_type = "hexagonal", seed = i)
#'   plot(new_cells, main = paste("Seed", i, sep=" "))
#' }
#' }
assign_polygons <- function(shape, new_polygons) {
        UseMethod("assign_polygons")
}

#' @rdname assign_polygons
#' @export
assign_polygons.SpatialPolygonsDataFrame <- function(shape, new_polygons) { # nolint

  as(assign_polygons(sf::st_as_sf(shape), new_polygons), "Spatial")

}

#' @rdname assign_polygons
#' @export
assign_polygons.sf <- function(shape, new_polygons) {

  if (!inherits(new_polygons, "geogrid"))
    stop("\"new_polygons\" must be an object obtained ",
      "from calling calculate_grid().")

  new_shape <-
    sf::st_transform(
      sf::st_as_sf(new_polygons[[2]]),
      sf::st_crs(shape))

  # Cost matrix:
  # Distance from Original Polygon Centroid to Grid Polygon Centroid
  cost <-
    units::drop_units(
        sf::st_distance(
          sf::st_centroid(sf::st_as_sf(sf::st_geometry(shape))),
          sf::st_centroid(new_shape)
          )
        )

  min_matrix <- hungarian_cc(cost)

  min_idx <- apply(min_matrix, MARGIN = 1, FUN = which.max)

  out_shape <- sf::st_set_geometry(shape, sf::st_geometry(new_shape[min_idx, ]))

  out_shape

}
