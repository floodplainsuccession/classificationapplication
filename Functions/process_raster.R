process_raster <- function(raster) {
  if (raster@file@nbands==4)
  {
raster <- aggregate(raster, 4, fun = mean) #process raster for lower resolution (speeds up computing time)
raster@crs <-  CRS("+init=epsg:28992")
names(raster) <- c("blue", "green", "red", "NIR")
ndvi <- overlay(raster$NIR, raster$red, fun=function(x,y){(x-y)/(x+y)})
raster <- addLayer(raster, ndvi)
names(raster)[length(names(raster))]<-"ndvi"
}
return(raster)
  
  
}