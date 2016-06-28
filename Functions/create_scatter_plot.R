create_scatter_plot <- function(valuetable, class_names,classes_colours, nclasses,year){

class_values_list <- list()
for (i in  1:nclasses){
  
  values_class <- subset(valuetable, class == i)
  name<-class_names[i]
  class_values_list[name] <- list(values_class)
  
  
}
plot(NIR ~ red, data = class_values_list[[1]], pch = ".", col = classes_colours[[1]],
     cex=1.5,xlim=c(0,250),ylim=c(0,250),main=paste0("Scatter plot training data ",year), 
     ylab = "NIR band pixel value", xlab = "Red band pixel value")

for (i in 2:nclasses){
points(NIR ~ red, data = class_values_list[[i]], pch = ".", col = classes_colours[[i]],cex=1.5)
}
legend("bottomleft", legend=class_names[1:nclasses], fill=classes_colours, bg="white",cex=1)
}

