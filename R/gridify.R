gridify <- function(original_polygons, grid_type, method, learning_rate){
  
  original_shapes <- read_polygons(original_polygons)
  
  original_details <- get_shape_details(original_shapes)
  
  new_cells <-  calculate_cell_size(original_shapes, original_details,learning_rate, grid_type)
  
  result <- assign_polygons(original_shapes,new_cells, method)
  
  return(result)
}