context("general")

test_that("general usage works", {

  input_file <- system.file("extdata", "london_LA.json", package = "geogrid")
  original_shapes <- read_polygons(input_file)

  expect_s4_class(original_shapes, "SpatialPolygonsDataFrame")
  expect_length(original_shapes, 33)

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
  expect_s4_class(grid_shapes, "SpatialPolygonsDataFrame")
  expect_length(grid_shapes, 33)
  # should test the content of grid_shapes to ensure it contains what we expect
})
