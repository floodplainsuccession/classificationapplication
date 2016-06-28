
visualize_results_sep_leaflet <- function(difference_raster,change_colours,classes,nclasses){
  changes <- change_colours[[3]]
  
  difference_raster<-projectRaster(difference_raster,crs=CRS("+init=epsg:3857"),method="ngb")
  
rclmat<- as.matrix(cbind(1:99,rep(0,99)))               
counter<-2
base<-2
index<-1
rcl_matices<-list()
list_change_rasters<-list()
for (i in 2:(length(changes)-1)){
    if (substr(as.character(changes[i+1]),1,1)!=substr(as.character(changes[i]),1,1)){
  rclmat_class<-changes[base:counter]
  rcl_matices[classes[[index]]$clasname]<-list(rclmat_class)
  
  rclmat[rclmat_class,2]<-rclmat_class
  change_raster<-reclassify(difference_raster,rclmat)
  list_change_rasters[classes[[index]]$clasname] <- change_raster
  index<-index+1
  base<-counter+1
  
  
    }
  counter<-counter+1
  rclmat<- as.matrix(cbind(1:99,rep(0,99)))
}

# Define colors to use for vegetaion change classes
color_ramp_vec<-c()
for (i in rcl_matices){
  
  colorramp<-colorRampPalette(c("blue","orange","darkred"))(length(i))
  for (j in i){
    year1<-as.numeric(substr(as.character(j),1,1))
    year2<-as.numeric(substr(as.character(j),2,2))
    if (year1==year2){
      colorramp[i==j]<-"green"

    }
  }
  color_ramp_vec <- c(color_ramp_vec,colorramp)
}


pal1 = colorFactor(color_ramp_vec,
                   domain = change_colours[[3]][2:(length(change_colours[[3]])-1)],
                   na.color = "transparent")
layers<-c()
for (i in classes){
layers<-c(layers,paste0("Changes in ",i$clasname))
}

map_of_differences = leaflet()
  
  #Add Basemap
  map_of_differences<-addProviderTiles(map_of_differences,"OpenStreetMap.BlackAndWhite", group = "Openstreetmap") 
  map_of_differences<-addProviderTiles(map_of_differences,"Stamen.TonerLite", group = "Stamen Toner Lite")
  map_of_differences<-addProviderTiles(map_of_differences,"Stamen.Watercolor", group = "Stamen Watercolor")
# 
for (i in 1:nclasses){

  map_of_differences<-addRasterImage(map_of_differences,list_change_rasters[[i]],
               group = layers[i],
               colors = pal1,
               project = FALSE)
}

   # Add switch between layers
map_of_differences<-addLayersControl(map_of_differences,
                                     baseGroups = c("Openstreetmap", "Stamen Toner Lite", "Stamen Watercolor"),
                                     overlayGroups = layers,
    options = layersControlOptions(collapsed = FALSE))

  # Add change legend
  map_of_differences<- addLegend(map_of_differences,"bottomleft",
            colors=color_ramp_vec,
            labels = change_colours[[1]],
            values = change_raster, title="Before -> After",
            opacity=1)
  map_of_differences <- hideGroup(map_of_differences,layers)
  return(map_of_differences)
}

