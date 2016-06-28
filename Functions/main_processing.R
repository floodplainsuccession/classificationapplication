## Classifying vegetation in the Dutch flood plains
## Deirdre Bosch, Maggie Mulley,Felix Thomas, Mark ten Vregelaar and Aline Werner
## WUR GRS-60312 Remote Sensing and GIS Integration
## 15-06-2016

main_processing <- function(input_List) {

  
 # create window for updating  
  w <- gwindow("Running the script")
  label <- glabel("The script is running... be patient :)", container = w)
  bar <- gstatusbar("Loading libraries...", container = w)
  
  # Import modules
  library(raster)
  library(rgdal)
  library(randomForest)
  library(leaflet)
  library(rLiDAR)
  library(htmlwidgets)
  
  # source functions
  source('Functions/getClasses.R')
  source('Functions/process_raster.R')
  source('Functions/generate_RFmodel.R')
  source('Functions/visualize_results_leaflet.R')
  source('Functions/get_changes.R')
  source('Functions/addLidar.R')
  source('Functions/get_change_stat.R')
  source('Functions/get_colorramp.R')
  source('Functions/visualize_classes.R')
  source('Functions/visualize_differences_leaflet.R')
  source('Functions/visualize_results_sep_leaflet.R')
  source('Functions/create_scatter_plot.R')
  
  # Create directories
  data_dir = 'Data'
  output_dir = 'Output'
  functions_dir = 'Functions'
  dir_list = list(data_dir, functions_dir, output_dir)
  for (i in dir_list)
  {
    if (!file.exists(i)) {
      dir.create(i)
    }
  }
  svalue(bar) <- paste("Generating list...")
  #Get user input
  name_raster1 <- input_List[[1]]
  name_raster2 <- input_List[[2]]
  
  Lidar1 <- input_List[[3]]
  Lidar2 <- input_List[[4]]
  
  poly1 <- input_List[[5]]
  poly2 <- input_List[[6]]
  
  plotbars <- input_List[[7]]
  saveresults <- input_List[[8]]
  showRFmodel <- input_List[[9]]
  
  classes_colours <- c("lightgreen", "peru", "orange", "violet", "royalblue","grey","aquamarine2","chartreuse")#
  
  
  ## process data year 1
  svalue(bar) <- paste("Processing raster 1...")
  print("test1")
  raster1 <- brick(paste0("Data/", name_raster1,".tif"))
  print("test2")
  training_poly1 <- readOGR('Data', poly1)
  
  training_poly1$code <- as.numeric(training_poly1@data$class)
  classes_poly1 <- getClasses(training_poly1)
  nclasses_poly1 <- length(classes_poly1)
  class_names_1 <- c()
  for (i in classes_poly1) {
    class_names_1 <- c(class_names_1, i$clasname)
  }
  raster1 <- process_raster(raster1)
  
  # add LIDAR BANDS
  if ((nchar(Lidar1)) > 0) {
    svalue(bar) <- paste("Processing LiDAR...")
    LIDARpoints <- readLAS(paste0("Data/", Lidar1, ".las"))
    raster1 <- addLIDAR(raster1, LIDARpoints)
  }
  # Generate classification model
  svalue(bar) <- paste("Generating classification model...")
  list_raster1_model <- generate_RFModel(raster1, training_poly1, nclasses_poly1)
  raster1_model<-list_raster1_model[[1]]
  if (showRFmodel){
    dev.new()
    create_scatter_plot(list_raster1_model[[2]], class_names_1,classes_colours, nclasses_poly1,"year 1")
    rownames(raster1_model$confusion)[1:nclasses_poly1]<-class_names_1
    colnames(raster1_model$confusion)[1:nclasses_poly1]<-class_names_1
    #dev.new()
    ##varImpPlot(raster1_model,main = "Variable importance plot year 1")
    confR1<<-raster1_model$confusion
  }
  # Classify raster
  svalue(bar) <- paste("Classifying rasters...")
  raster1_class <- predict(raster1, model = raster1_model, na.rm = TRUE)
  raster1_class<-focal(raster1_class,w=matrix(1,3,3),fun=modal)
  ##process data year 2
  if (nchar(poly2)==0){
    poly2 <- poly1
  }
  if (nchar(name_raster2)>0){
    svalue(bar) <- paste("Loading second datasets...")
    raster2 <- brick(paste0("Data/", name_raster2, ".tif"))
    training_poly2 <- readOGR('Data', poly2)
    training_poly2$code <- as.numeric(training_poly2@data$class)
    classes_poly2 <- getClasses(training_poly2)
    nclasses_poly2 <- length(classes_poly2)
    class_names_2 <- c()
    for (i in classes_poly2) {
      class_names_2 <- c(class_names_2, i$clasname)
    }
    raster2 <- process_raster(raster2)
    
    if ((nchar(Lidar2)) > 0) {
      svalue(bar) <- paste("More LiDAR data...yay")
      LIDARpoints <- readLAS(paste0("Data/", Lidar2, ".las"))
      raster1 <- addLIDAR(raster1, LIDARpoints)
    }
    list_raster2_model <- generate_RFModel(raster2, training_poly2, nclasses_poly2)
    raster2_model<-list_raster2_model[[1]]
    if (showRFmodel){
      rownames(raster2_model$confusion)[1:nclasses_poly2]<-class_names_2
      colnames(raster2_model$confusion)[1:nclasses_poly1]<-class_names_2
      dev.new()
      create_scatter_plot(list_raster2_model[[2]], class_names_2,classes_colours, nclasses_poly2,"year 2")
      #dev.new()
      ##varImpPlot(raster2_model,main = "Variable importance plot year 2")
      confR2<<-raster2_model$confusion
      }
    raster2_class <- predict(raster2, model = raster2_model, na.rm = TRUE)
    raster2_class<-focal(raster2_class,w=matrix(1,3,3),fun=modal)
    ## comapre the two years
    
    
    changes_list <- get_changes(raster1_class, raster2_class)
    # basic statistic differences
    change_statistics <-
      get_change_stat(changes_list[[2]], changes_list[[1]],nclasses_poly1,class_names_1,class_names_2)

    
    if (plotbars){
      valuelabels = c(0.7, 1.9)
      col = c("lightgreen", "red")
      dev.new()
      barplot(change_statistics[[2]], col = col, main = "Stability of Vegetation",
              names.arg = c("Stable", "Unstable"), ylim=c(0,100))
      text(valuelabels, change_statistics[[2]], label=change_statistics[[2]], pos=1, offset = -1, srt=0)
    }
    
    
    #visualize result in leaflet
    svalue(bar) <- paste("Making maps...")
    leaflet_classes <- visualize_results_leaflet(
      raster1_class,
      raster2_class,
      changes_list[[2]],
      class_names_1,
      classes_colours,
      nclasses_poly1)
    
    
    
    # show map
    
    # create colours and labels for visulisation of results
    colour_labels <- get_colorramp(changes_list[[1]],classes_poly1,classes_poly2)
    # generate leaflet viewer
    leaflet_differences <- visualize_differences_leaflet(changes_list[[2]],changes_list[[1]],colour_labels)
    # show map
    
    
    # create leaflet to show differences
    
    difference_raster<-changes_list[[1]]
    changes<-colour_labels[[3]]
    classes<-classes_poly1
    nclasses<-nclasses_poly1
    
    leaflet_separate <- visualize_results_sep_leaflet(changes_list[[1]],colour_labels,classes_poly1,nclasses_poly1)
    
    
    
    # save result in output folder
    if (saveresults) {
      svalue(bar) <- paste("Saving files...")
      writeRaster(
        raster1_class,
        filename = "Output/raster1_class.tif",
        format = "GTiff",
        overwrite = T
      )
      writeRaster(
        raster2_class,
        filename = "Output/raster2_class.tif",
        format = "GTiff",
        overwrite = T
      )
      writeRaster(
        changes_list[[2]],
        filename = "Output/changes_binary.tif",
        format = "GTiff",
        overwrite = T
      )
      writeRaster(
        changes_list[[1]],
        filename = "Output/changes.tif",
        format = "GTiff",
        overwrite = T
      )
      #return(list(leaflet_classes, leaflet_differences, leaflet_separate))
    }
    
    saveWidget(leaflet_classes, file="leaflet_classes.html",selfcontained = F)
    saveWidget(leaflet_differences, file="leaflet_differences.html",selfcontained = F)
    saveWidget(leaflet_separate, file="leaflet_separate.html",selfcontained = F)
    
  } else {
    leaflet_classes <- visualize_classes(
      raster1_class,
      class_names_1,
      classes_colours,
      nclasses_poly1)
    
    
    if (saveresults) {
      svalue(bar) <- paste("Saving files...")
      writeRaster(
        raster1_class,
        filename = "Output/raster1_class.tif",
        format = "GTiff",
        overwrite = T
      )
    }
    saveWidget(leaflet_classes, file="leaflet_classes.html",selfcontained = F)
  }
  svalue(bar) <- paste("Done!")
  dispose(w)
}