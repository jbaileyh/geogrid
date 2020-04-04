context("general")

test_that("general usage works", {

  input_file <- system.file("extdata", "london_LA.json", package = "geogrid")
  # This generates a warning telling us that using the CRS argument in this
  # way simply replaces the CRS attribute and does not transform/modify any
  # coordinates. Because this is what we want, we suppress the warnings.
  suppressWarnings(
    original_shapes <- sf::st_read(input_file, crs = 27700)
    )

  expect_s3_class(original_shapes, "sf")
  expect_equal(nrow(original_shapes), 33)

  new_cells <- calculate_grid(shape = original_shapes,
    grid_type = "hexagonal", seed = 1)

  expect_s3_class(new_cells, "geogrid")
  expect_s4_class(new_cells[[1]], "SpatialPoints")
  expect_s4_class(new_cells[[2]], "SpatialPolygons")
  expect_length(new_cells[[2]], 33)

  crds <- round(as.numeric(sp::coordinates(new_cells[[2]])[, 1]), 1)
  comp <- c(533504.4, 540491, 523024.4, 530011, 536997.7, 543984.3, 512544.4,
    519531.1, 526517.7, 533504.4, 540491, 547477.7, 516037.7, 523024.4,
    530011, 536997.7, 543984.3, 550971, 512544.4, 519531.1, 526517.7,
    533504.4, 540491, 547477.7, 554464.3, 516037.7, 523024.4, 530011,
    536997.7, 543984.3, 550971, 526517.7, 533504.4)
  expect_equivalent(crds, comp)

  grid_shapes <- assign_polygons(original_shapes, new_cells)
  expect_s3_class(grid_shapes, "sf")
  expect_equal(nrow(grid_shapes), 33)
  # should test the content of grid_shapes to ensure it contains what we expect
})
