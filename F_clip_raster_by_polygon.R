# Clip rasters by polygon
clip.by.polygon <- function(raster, # Raster object
                            shape # Polygon object
                            ) {
  
  a1_crop<-crop(raster, shape)
  
  step1<-rasterize(shape, a1_crop)
  
  a1_crop*step1
}