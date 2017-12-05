export_hexjson <- function(assigned_polygons, new_cells_hex)
  
{
  coords <- coordinates(assigned_polygons)
  coord_box <- as.matrix(bbox(coords))

  boxmatrix <- matrix(nrow = 5, ncol = 2)
  boxmatrix[1,] <- coord_box[,1]
  boxmatrix[2,] <- c(coord_box[1,1],coord_box[2,2])
  boxmatrix[3,] <- coord_box[,2]
  boxmatrix[4,] <- c(coord_box[1,2],coord_box[2,1])
  boxmatrix[5,] <- coord_box[,1]
    
    
  spacing <- new_cells_hex[[3]]
  
  p <- Polygon(xym)
  ps <- Polygons(list(p),1)
  sps <- SpatialPolygons(list(ps))

  fullgrid <- spsample(sps, type = "hexagonal", cellsize = spacing, FALSE)
  
  nrows_hex = length(unique(coords[,2]))
  ncols_hex = length(unique(coords[,2]))
  r <- raster(ncols = ncols_hex, nrows = nrows_hex)
  hex_ras<-rasterize(assigned_polygons, r, "key_new")

  
}

#Approaches
#- create a matrix of the relevant dimensions
#- loop through the items from north to south west to east and assign each polygon to its grid location. 

#- or rasterise, extract the matrix of the raster.  

#Extract existing grid
#Generate new grid from corner points with same spacing. (method doesn't instantiate from same location)
#Could translate to account for this. 
#Assign location to each value (and other variables)
#Turn into HexJSON
