require(sp)
require(rgdal)
require(raster)
require(fields)
setwd("D:/CLIMATE/StationNormals")

# FIT THIN PLATE SPLINE		
MinTemp <- readOGR(getwd(), "MinTemp")  
x<-as.matrix(cbind(coordinates(MinTemp),ELEV=MinTemp@data$ELEV))
  dec.min.temp <- Tps(x=x, Y=MinTemp@data$DEC) 		 
# dec.min.temp <- Tps(x=coordinates(MinTemp), Y=MinTemp@data$DEC, Z=MinTemp@data$ELEV)    
ELEV <- raster("D:/GIS/US/GLOBE30/GLOBE30.img")
  interpolate(ELEV, dec.min.temp, filename="DecMinTemp.img", fun=predict)
 