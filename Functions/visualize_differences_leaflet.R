visualize_differences_leaflet<-function(stability_raster,change_raster,list_colour_labels){

  stability_raster<-projectRaster(stability_raster,crs=CRS("+init=epsg:3857"),method="ngb")
  change_raster<-projectRaster(change_raster,crs=CRS("+init=epsg:3857"),method="ngb")
# Define colors to use for vegetaion change classes
pal1 = colorFactor(list_colour_labels[[2]],
                   domain = list_colour_labels[[3]][2:(length(list_colour_labels[[3]])-1)],
                   na.color = "transparent")

# Define colors to use for vegetation stability
pal2 = colorFactor(c("lightgreen", "orangered"),
                   domain = (c("1", "2")),
                   na.color = "transparent")

map_of_differences = leaflet()%>%
  
  #Add Basemap
  addProviderTiles("OpenStreetMap.BlackAndWhite", group = "Openstreetmap") %>%
  addProviderTiles("Stamen.TonerLite", group = "Stamen Toner Lite")%>%
  addProviderTiles("Stamen.Watercolor", group = "Stamen Watercolor")%>%
  
  # Add vegetation map to map
  addRasterImage(change_raster, 
                 group = "Vegetation Change", 
                 colors = pal1, 
                 project = FALSE)%>%
  
  # Add stable/unstable to map
  addRasterImage(stability_raster,
                 group ="Vegetation Stability",
                 colors = pal2,
                 project = FALSE)%>%
  
  # Add change legend
  addLegend("bottomleft",
            colors=list_colour_labels[[2]],
            labels = list_colour_labels[[1]],
            values = change_raster, title="Before -> After",
            opacity=1)%>%
  
  # Add stability legend
  addLegend("bottomleft",
            colors=c("lightgreen", "orangered"),
            labels = c("stable", "unstable"),
            values = stability_raster, title="Vegetation Stability",
            opacity=1)%>%
  
  # Add switch between layers
  addLayersControl(baseGroups = c("Openstreetmap", "Stamen Toner Lite", "Stamen Watercolor"),
    overlayGroups = c("Vegetation Stability","Vegetation Change"
                      ),
    options = layersControlOptions(collapsed = FALSE))

# return map
return(map_of_differences)
}