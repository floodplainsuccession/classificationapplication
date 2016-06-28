get_colorramp <- function(change_raster,classes_polygon1,classes_polygon2){
  
colour_ramps <- list(c("lightgreen","darkgreen"),c("moccasin","sienna"),c("gold","red"),c("lightpink","violetred"),c("lightcyan","royalblue"),
                     c("black","white"),c("aquamarine","aquamarine4"),c("chartreuse","chartreuse4")) 

changes <- c(freq(change_raster)[,1],99)
colours_changes<-c()
label_list<-c()
counter=1
n=1
for (i in 2:(length(changes)-1)){
  if (substr(as.character(changes[i+1]),1,1)==substr(as.character(changes[i]),1,1)){
    counter <- counter+1
  }else{
    colours_changes<-c(colours_changes,colorRampPalette(colour_ramps[[n]])(counter))
    counter <- 1
    n <- n+1
  }
  year1<-as.numeric(substr(as.character(changes[i]),1,1))
  year2<-as.numeric(substr(as.character(changes[i]),2,2))
  
  string_year1<-classes_polygon1[[year1]]$clasname
  string_year2<-classes_polygon2[[year2]]$clasname
  label<-paste(string_year1 , "->", string_year2)
  label_list<-c(label_list,label)
}
return(list(label_list,colours_changes,changes))
}