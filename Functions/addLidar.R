addLIDAR<- function(raster,Lidar){
  
  prj_string_RD <- CRS("+init=epsg:28992")
  points_df <- SpatialPointsDataFrame(Lidar[,1:2],data=data.frame(Lidar[,3]),proj4string =prj_string_RD)
  
  points_count<-rasterize(points_df,raster,fun="count")
  points_max<-rasterize(points_df,raster,fun="max")
  points_sd<-rasterize(points_df,raster,fun=function(x,...)sd(x))
  
  points_count[is.na(points_count)] <- 0
  points_max[is.na(points_max)] <- 0
  points_sd[is.na(points_sd)] <- 0
  
  points_count<-mask(points_count,raster[[1]])
  points_max<-mask(points_max,raster[[1]])
  points_sd<-mask(points_sd,raster[[1]])
  
  
  
  raster_LIDAR<-brick(list(raster,points_count[[2]],points_max[[2]],points_sd[[2]]))
  names(raster_LIDAR)[(length(names(raster_LIDAR))-2):length(names(raster_LIDAR))]<-c("LIDAR_counts","LIDAR_max","LIDAR_sd")
  return(raster_LIDAR)
}