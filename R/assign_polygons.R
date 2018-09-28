#' Assign the polygons in the original spatial data to their new location.
#'
#' Assigns each polygon in the original file to a new location in the gridded geometry using the Hungarian algorithm.
#'
#' @param shape A "SpatialPolygonsDataFrame" or an sf object representing the original spatial polygons.
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
#'   new_cells <- calculate_grid(shape = original_shapes, grid_type = "hexagonal", seed = i)
#'   plot(new_cells, main = paste("Seed", i, sep=" "))
#' }
#' }
assign_polygons <- function(shape, new_polygons){
        UseMethod("assign_polygons")
}

#' @rdname assign_polygons
#' @export
assign_polygons.SpatialPolygonsDataFrame <- function(shape, new_polygons) {
  original_points <- rgeos::gCentroid(shape, byid = TRUE)
  shape@data$CENTROIX <- original_points$x
  shape@data$CENTROIY <- original_points$y
  shape@data$key_orig <- paste(original_points$x, original_points$y, sep = "_")

  if (!inherits(new_polygons, "geogrid"))
    stop("'new_polygons' must be an object obtained ",
      "from calling calculate_grid().")

  new_points <- new_polygons[[1]]
  vector_length <- length(shape)

  new_polygons2 <- new_polygons[[2]]
  # polygon_points <- rgeos::gCentroid(new_polygons2, byid = TRUE)
  s_poly <- sp::SpatialPolygonsDataFrame(
    new_polygons2, as.data.frame(sp::coordinates(new_polygons2)))
  s_poly$key_new <- paste(s_poly@data$V1, s_poly@data$V2, sep = "_")

  # Define these vectors, used in the assignment loop.
  closest_site_vec <- vector(mode = "numeric", length = vector_length)
  min_dist_vec <- vector(mode = "numeric", length = vector_length)
  taken_vec <- vector(mode = "numeric", length = vector_length)
  taken_vec_index <- integer(vector_length)

  # shape_areas <- rgeos::gArea(shape, byid = TRUE)

  for (i in 1:vector_length) {
    dist_vec <- sp::spDistsN1(original_points, new_points[i], longlat = FALSE)
    min_dist_vec[i] <- min(dist_vec)

    if (i > 1) {
      dist_vec[taken_vec_index] <- NA
      closest_site_vec[i] <- which.min(dist_vec)
    } else {
      closest_site_vec[i] <- which.min(dist_vec)
    }

    taken_vec[i] <- which.min(dist_vec)
    taken_vec_index <- taken_vec[taken_vec > 0]

    costmatrix <- sp::spDists(original_points, new_points, longlat = FALSE)
    colnames(costmatrix) <- paste(s_poly@data$V1, s_poly@data$V2, sep = "_")
    rownames(costmatrix) <- paste(original_points@coords[, 1],
      original_points@coords[, 2], sep = "_")
    hungarian_costmin <- hungarian_cc(costmatrix)
  }

  costmin_locs <- as.data.frame(which(hungarian_costmin == 1, arr.ind = TRUE))
  costmin_locs$key_new <- colnames(costmatrix)[costmin_locs$col]
  costmin_locs$key_orig <- rownames(costmatrix)[costmin_locs$row]
  # val <- strsplit(costmin_locs$key_new, "_")
  # costmin_locs$CENTROIDX <- as.numeric(val[[1]][1])
  # costmin_locs$CENTROIDy <- as.numeric(vsl[[1]][1])

  final_table <- costmin_locs

  combi <- sp::merge(shape@data, final_table, by.x = "key_orig")
  combi2 <- sp::merge(s_poly, combi, by.x = "key_new")
  return(combi2)
}

#' @rdname assign_polygons
#' @export
assign_polygons.sf <- function(shape, new_polygons){
        st_as_sf(assign_polygons(as(shape, "Spatial"), new_polygons))
}
