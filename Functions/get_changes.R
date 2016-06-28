get_changes <- function(year_1,year_2){
  
  
  #  Multiply year 1 by 10
  sample_year_1 = calc(year_1, function(x) {x*10})
  
  # Create and save the vegetation change map as a .tiff in working directory
  change = mosaic(sample_year_1, year_2, fun=sum)
  # 
  # n = c(11, 11, 12, 12, 13, 13, 14, 14, 15, 15,
  #       21, 21, 22, 22, 23, 23, 24, 24, 25, 25,
  #       31, 31, 32, 32, 33, 33, 34, 34, 35, 35,
  #       41, 41, 42, 42, 43, 43, 44, 44, 45, 45,
  #       51, 51, 52, 52, 53, 53, 54, 54, 55, 55,
  #       10, 0, 20, 0, 30, 0, 40, 0, 50, 0) 
  
  #rclmat = matrix(n, ncol=2, byrow=TRUE)
  #change = reclassify(change, rclmat)
  
  table11<-c()
  for (i in 1:9){
    table11<- c(table11,i*11)
  }
  
  rclmat<- as.matrix(cbind(1:99,rep(2,9)))
  rclmat[,2][table11] <- 1
  stable = reclassify(change, rclmat)
  
  # Define colors to use for vegetation stability
  pal2 = colorFactor(c("lightgreen", "orangered"),
                     domain = (c("1", "2")),
                     na.color = "transparent")


  return(list(change,stable))
}

