visualize_classes <- function(raster_classes,class_names,classes_colours,nclasses){
  
  raster_classes<-projectRaster(raster_classes,crs=CRS("+init=epsg:3857"),method="ngb")
  
  pal = colorFactor(classes_colours[1:nclasses], 
                    domain=(1:nclasses), na.color = "transparent")  
  
  # Build Map
  mymap = leaflet()%>%
    
  addProviderTiles("OpenStreetMap.BlackAndWhite", group = "Openstreetmap") %>%
  addProviderTiles("Stamen.TonerLite", group = "Stamen Toner Lite")%>%
  addProviderTiles("Stamen.Watercolor", group = "Stamen Watercolor")%>%
    
    # Add vegetation map to map
    addRasterImage(raster_classes, 
                   group="Classification result", 
                   colors=pal, 
                   project=FALSE)%>%
    
    # Add switch between layers
    addLayersControl(baseGroups = c("Openstreetmap", "Stamen Toner Lite", "Stamen Watercolor"),
      overlayGroups = c("Classification result"),
      options = layersControlOptions(collapsed = FALSE)) %>%
    
    # Add legend
    addLegend("bottomleft", 
              colors=classes_colours[1:nclasses],
              labels = class_names, 
              title="Vegetation Class",
              opacity=1)
  
  # print map
  return(mymap)
  
}