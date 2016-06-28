GUI_setup <- function()
{

  ### open window
  source('Functions/main_processing.R')
  options(guiToolkit =  "tcltk")
  
  win <- gwindow("Classifying Vegetation")
  
  ### giving the "enter text" option a go...
  grp_name_upload <- ggroup(horizontal = T, container = win, spacing = 10)
  img_f1 <- glabel("Upload Raster File 1: ", container = grp_name_upload)
  image_file_name1 <- gedit(initial.msg = "type file name", container = grp_name_upload)
  # addHandlerChanged(image_file_name1, handler = function(h,...)
  #  {raster1 <<- svalue(h$obj)})
  addSpring(grp_name_upload)
  img_f2 <- glabel("Upload Raster File 2 (optional): ", container = grp_name_upload)
  image_file_name2 <- gedit(initial.msg = "type file name", container = grp_name_upload)
  
  ### for LiDAR
  grp_LiDAR_upload <- ggroup(horizontal = T, container = win, spacing = 10)
  LiDAR_f1 <- glabel("Upload LiDAR File 1 (optional): ", container = grp_LiDAR_upload)
  LiDAR_file_name1 <- gedit(initial.msg = "type file name", container = grp_LiDAR_upload)
  # addHandlerChanged(image_file_name1, handler = function(h,...)
  #  {raster1 <<- svalue(h$obj)})
  addSpring(grp_LiDAR_upload)
  LiDAR_f2 <- glabel("Upload LiDAR File 2 (optional): ", container = grp_LiDAR_upload)
  LiDAR_file_name2 <- gedit(initial.msg = "type file name", container = grp_LiDAR_upload)
  
  
  ### loading up the shapefiles
  grp_shp_upload <- ggroup (horizontal = T, container = win, spacing = 10)
  poly_f1 <- glabel("Upload Training Shapefile 1: ", container = grp_shp_upload)
  polygon_name1 <- gedit(initial.msg = "type file name", container = grp_shp_upload)
  #addHandlerChanged(polygon_name1, handler = function(m,...)
  # {training1 <<- svalue(m$obj)})
  addSpring(grp_shp_upload)
  poly_f2 <- glabel("Upload Training Shapefile 2 (optional): ", container = grp_shp_upload)
  polygon_name2 <- gedit(initial.msg = "type file name", container = grp_shp_upload)
  
  ### tick boxes
  grp_tick <- ggroup(container = win)
  addSpring(grp_tick)
  
  plotbars <- F
  saveresults <- F
  showRFmodel <- F
  
  bar_tick_box <- gcheckbox("Plot bar graphs", checked = FALSE, container=grp_tick, handler = function(h,...)
  {plotbars <<- svalue(bar_tick_box)})
  
  save_tick_box <- gcheckbox("Save files", checked = FALSE, container=grp_tick, handler = function(h,...)
  {saveresults <<- svalue(save_tick_box)})
  
  RF_tick_box <- gcheckbox("Show Random Forest results", checked = FALSE, container=grp_tick, handler = function(h,...)
  {showRFmodel <<- svalue(RF_tick_box)})
  
  stat_bar <- gstatusbar("Load files", container=win)
  
  ### Run button
  grp_testbutton <- ggroup(container = win, horizontal = FALSE)
  run_class <- gbutton("Run", container = grp_testbutton, handler = function(h,...)
                       {user_entry <<- list(svalue(image_file_name1), svalue(image_file_name2),
                                           svalue(LiDAR_file_name1), svalue(LiDAR_file_name2),
                                           svalue(polygon_name1),svalue(polygon_name2))
                       
                       tick_box_list <<- list(plotbars, saveresults, showRFmodel)
                         file_name <- c("Raster File 1", "Raster File 2", "LiDAR File 1", "LiDAR File 2", "Training Shapefile 1", "Training Shapefile 2")
                         wrong_entry <- c()
                         counter <- 0
                         input_vec<-c()
                         error <- F
                         for (i in user_entry){
                           counter <- counter + 1
                           {if(!(file.exists(paste0("Data/", i, ".tif"))|file.exists(paste0("Data/", i, ".las"))|file.exists(paste0("Data/", i, ".shp"))) & nchar(i) > 0)
                           {wrong_entry <- c(wrong_entry, i)
                           input_vec <- c(input_vec,file_name[counter])
                           error <- T}}
                         }
                         if(error)
                         {svalue(stat_bar) <- paste("Reload Files")
                           window <- gwindow("Incorrect File")
                         grp_err <- ggroup(container = window, horizontal = FALSE)
                         counter2 <- 0
                         for (i in wrong_entry)
                         {counter2 <- counter2 + 1
                         glabel(paste("There is no file called", i, "for", input_vec[counter2]), container = grp_err)}
                         gbutton("OK", container=grp_err, handler = function(h,...) dispose(window))}
                         else
                         {
                         svalue(stat_bar) <- paste("Running")
                         correct_List <<- list(svalue(image_file_name1), svalue(image_file_name2),
                                               svalue(LiDAR_file_name1), svalue(LiDAR_file_name2),
                                               svalue(polygon_name1),svalue(polygon_name2),
                                               plotbars, saveresults, showRFmodel)

                         tryCatch(main_processing(input_List = correct_List),
                                  
                                  error = function(err) {
                                    errorwin<-gwindow("error window")
                                    glabel(paste("Unfortunately, The program has encountered an error:\n",err),container = errorwin)
                                    stop()
                                  }
                         )
                         
                        browseURL("leaflet_classes.html")

                        if(nchar(correct_List[[2]]) > 0)
                        {
                          browseURL("leaflet_differences.html")
                          browseURL("leaflet_separate.html")}
                        
                        if(exists("confR1"))
                        {tabwin <- gwindow()
                        names <- rownames(confR1)
                        confR1 <- cbind(names,confR1)
                        grp_tabl <- ggroup (container = tabwin, horizontal = F)
                        tab_lab <- glabel("Confusion Matrix Image 1:", container= grp_tabl)
                        cnftableR1 <- gtable(confR1, container= grp_tabl)}
                        
                        if(exists("confR2"))
                        {
                        names <- rownames(confR2)
                        confR2 <- cbind(names,confR2)
                        grp_tabl2 <- ggroup (container = tabwin, horizontal = F)
                        tab_lab2 <- glabel("Confusion Matrix Image 2:", container= grp_tabl2)
                        cnftableR2 <- gtable(confR2, container= grp_tabl2)}
                        
                        if(exists("changematrix"))
                        {if(!exists("tabwin")){
                          tabwin <- gwindow()
                        }
                          names <- rownames(changematrix)
                          changematrix <- cbind(names,changematrix)
                          grp_tablchang <- ggroup (container = tabwin, horizontal = F)
                          tab_lab <- glabel("Change Matrix:", container= grp_tablchang)
                          chngtab <- gtable(changematrix, container= grp_tablchang)}
                         
                         svalue(stat_bar) <- paste("Finished")
  }}
                       
                       )

}



