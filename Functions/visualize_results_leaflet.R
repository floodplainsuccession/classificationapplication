visualize_results_leaflet <- function(raster_classes1,raster_classes2,change_raster,
                                      class_names,classes_colours,nclasses){
  
  raster_classes1<-projectRaster(raster_classes1,crs=CRS("+init=epsg:3857"),method="ngb")
  raster_classes2<-projectRaster(raster_classes2,crs=CRS("+init=epsg:3857"),method="ngb")
  
  pal = colorFactor(classes_colours[1:nclasses], 
                    domain=(1:nclasses), na.color = "transparent")
  # Define colors to use for vegetation stability
  pal2 = colorFactor(c("lightgreen", "orangered"),
                     domain = (c("1", "2")),
                     na.color = "transparent")
  
  # Build Map
  mymap = leaflet()%>%
    
    addProviderTiles("OpenStreetMap.BlackAndWhite", group = "Openstreetmap") %>%
    addProviderTiles("Stamen.TonerLite", group = "Stamen Toner Lite")%>%
    addProviderTiles("Stamen.Watercolor", group = "Stamen Watercolor")%>%
    
    # Add vegetation map 1 to map
    addRasterImage(raster_classes1, 
                   group="Classification year 1", 
                   colors=pal, 
                   project=FALSE)%>%
    # Add vegetation map2  to map
    addRasterImage(raster_classes2, 
                   group="Classification year 2", 
                   colors=pal, 
                   project=FALSE)%>%
    # Add stable/unstable to map
    addRasterImage(change_raster,
                   group ="Vegetation Stability",
                   colors = pal2,
                   project = FALSE)%>%
 
    
    # Add switch between layers
    addLayersControl(baseGroups = c("Openstreetmap", "Stamen Toner Lite", "Stamen Watercolor"),
      overlayGroups = c("Vegetation Stability","Classification year 2" ,"Classification year 1"),
      options = layersControlOptions(collapsed = FALSE)) %>%

    
    # Add legend
    addLegend("bottomleft", 
              colors=classes_colours[1:nclasses],
              labels = class_names, 
              title="Vegetation Classes",
              opacity=1)%>%
  # Add stability legend
  addLegend("bottomright",
            colors=c("lightgreen", "orangered"),
            labels = c("stable", "unstable"),
            values = veg_stable, title="Vegetation Stability",
            opacity=1)
  
  # print map
  return(mymap)
  
}