get_change_stat <- function(change_raster,differences,nclasses,clas_poly1,clas_poly2){
nclasses_pol2<- length(clas_poly2)
# Count stable and unstable pixels
freq_stable = freq(change_raster)
# Calculate percentage of stable and unstable pixels
n_stable = freq_stable [2,2]
n_unstable = freq_stable [3,2]
n_total = n_stable + n_unstable

per_stable = (n_stable/n_total)*100
per_unstable = (n_unstable/n_total)*100

# Plot distribution of stable and unstable
stability = round(c(per_stable, per_unstable), digits=2)
col = c("lightgreen", "red")



frequencies = c()

for (i in seq(10,nclasses*10,10)){
  for (j in (1:nclasses)){
    freq_change = freq(differences, digits = 2, value = (i+j), useNA = "no")
    frequencies = c(frequencies,freq_change)
  }
}

changematrix2 <- matrix(frequencies,ncol = nclasses_pol2)
colnames(changematrix2)<-clas_poly2
rownames(changematrix2)<-clas_poly1

total <- sum(changematrix2)
changematrix<-cbind(changematrix2,rowSums(changematrix2))
changematrix<-cbind(changematrix,rowSums(changematrix2)/total*100)
changematrix<-rbind(changematrix,colSums(changematrix2))
changematrix<-rbind(changematrix,colSums(changematrix2)/total*100)

colnames(changematrix)[(nclasses+1):(nclasses+2)] <- c("area_m2_year1","area_percent_year1") 
rownames(changematrix)[(nclasses_pol2+1):(nclasses_pol2+2)] <- c("area_m2_year2","area_percent_year2")
changematrix[(nclasses_pol2+1),(nclasses+1)]<-sum(changematrix[,nclasses_pol2+1][1:nclasses+1])
changematrix[(nclasses_pol2+2),(nclasses+2)]<-100
changematrix[(nclasses_pol2+1),(nclasses+2)]<-NA
changematrix[(nclasses_pol2+2),(nclasses+1)]<-NA
changematrix<<-round(changematrix,2)

return(list(changematrix,stability))

}