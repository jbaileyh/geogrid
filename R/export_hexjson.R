export_hexjson <- function(assigned_polygons)
  
{
  coords <- coordinates(assigned_polygons)
  xmin <- min(coords[,1])
  xmax <- max(coords[,1])
  ymin <- min(coords[,2])
  ymax <- min(coords[,2])
  spacing <- 
  
}


#Extract existing grid
#Generate new grid from corner points with same spacing. 
#Assign location to each value (and other variables)
#Turn into HexJSON
