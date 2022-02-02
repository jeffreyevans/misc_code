# Create boolean matrix where distance condition is <= d TRUE else FALSE
require(sp)
require(rgeos)
data(meuse)
  coordinates(meuse) <- ~x+y

d=200
DistMat <- rgeos::gWithinDistance(meuse, meuse, dist=d, byid=TRUE)  
  diag(DistMat) <- NA

# Join back to data
cids <- colnames(DistMat)
  DistMat <- as.data.frame(DistMat)
    names(DistMat) <- paste("NID", cids, sep=".")
      meuse@data <- data.frame(meuse@data, DistMat) 
