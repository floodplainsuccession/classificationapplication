### function to generate training pixel values and classify image

generate_RFModel <- function(covs, trainingPoly, nclasses) {
  nbands<-nlayers(covs)
  classes <- rasterize(trainingPoly, covs, field='code')
  covmasked <- mask(covs, classes)
  names(classes) <- "class"
  trainingbrick <- addLayer(covmasked, classes)
  valuetable <- getValues(trainingbrick)
  valuetable <- na.omit(valuetable)
  valuetable <- as.data.frame(valuetable)
  valuetable$class <- factor(valuetable$class, levels = c(1:nclasses))
  modelRF <- randomForest(x=valuetable[ ,1:nbands], y=valuetable$class,
                          importance = TRUE)
   return(list(modelRF,valuetable))
}



