#' geogrid
#'
#' Turn irregular polygons (such as geographical regions) into regular grids.
#'
#' @docType package
#' @author Joseph Bailey <jbailey@futurecities.catapult.org.uk>
#' @import Rcpp
#' @importFrom Rcpp evalCpp
#' @useDynLib geogrid
#' @exportPattern "^[[:alpha:]]+"
#' @name geogrid
#' @examples
#' input_file <- system.file("extdata", "london_LA.json", package = "geogrid")
#' original_shapes <- read_polygons(input_file)
#'
#' # calculate grid
#' new_cells <- calculate_grid(shape = original_shapes,
#'   grid_type = "hexagonal", seed = 1)
#' plot(new_cells)
#'
#' #
#' grid_shapes <- assign_polygons(original_shapes, new_cells)
#' par(mfrow=c(1, 2))
#' sp::plot(original_shapes)
#' sp::plot(grid_shapes)
#'
#' # look at different grids using different seeds
#' par(mfrow=c(2, 3), mar = c(0, 0, 2, 0))
#' for (i in 1:6) {
#'   new_cells <- calculate_grid(shape = original_shapes, grid_type = "hexagonal", seed = i)
#'   plot(new_cells, main = paste("Seed", i, sep=" "))
#' }
NULL
