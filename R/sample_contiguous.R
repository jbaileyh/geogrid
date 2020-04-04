#' Sample Contiguous
#'
#' @param shape sf shape object
#' @param n number of shapes to sample
#'
#' @return indices of a contiguous sample
#'
#' @export
#'
#' @examples
#' library(sf)
#' input_file <- system.file("extdata", "london_LA.json", package = "geogrid")
#' original_shapes <- st_read(input_file) %>% st_set_crs(27700)
#' sample_shapes <- sample_contiguous(original_shapes, n = 5)
sample_contiguous <- function(shape, n = 5) {

  if (!is(shape, "sf")) {
    stop("shape must be an sf object")
  }

  if (nrow(shape) < n) {
    stop("shape must have at least n records!")
  }

  idx <- .sample_contiguous(shape = shape, n = n, d = 0L, max_depth = 20L)

  shape %>% dplyr::slice(idx)

}

.sample_contiguous <- function(shape, n, d = 0L, max_depth = 20L) {

  idx <- sample.int(n = nrow(shape), size = 1)

  while (length(idx) < n) {
    new_idx <- shape %>%
                dplyr::slice(idx) %>%
                sf::st_touches(shape) %>%
                purrr::flatten_int() %>%
                unique() %>%
                dplyr::setdiff(idx)

    if (length(new_idx) == 0L && d < max_depth) { # If there's nothing new to add, start over
      idx <- .sample_contiguous(shape, n = n, d = d + 1L)
    } else if (d >= max_depth) {
      stop("Reached maximum retries: ", max_depth, ". Are your shapefiles sufficiently connected?")
    } else {
      idx <- c(idx,
             sample(x = new_idx,
                    size = min(length(new_idx),
                               n - length(idx)
                               )
                    )
              ) %>%
        sort()
    }
  }

  idx

}
