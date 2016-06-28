getClasses <- function(polygon) {
  code_numbers<-unique(polygon$code)
  
  classes<-list()
    for (i in sort(code_numbers)){
    class<- as.character(subset(polygon$class,polygon$code==i)[1])
    name <- paste0('class:',i)
    classes[[name]]<- list(code=i,clasname=class)
  }
  return(classes)
}
