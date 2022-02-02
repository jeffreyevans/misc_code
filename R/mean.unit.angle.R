######################################################
# x aspect or slope degrees or radians, 
# y sp polygons, 
# max.angle 90 for slope, 360 for aspect 
mean.unit.angle <- function(x, y, degrees = FALSE, max.angle = 360) {
  mang <- function(x,y,maxangle=max.angle) { (((450 - (atan2(x,y) * 57.296)) * 100) %% maxangle) / 100 }
  if(degrees == TRUE) { x <- calc(x, fun=function(x) { x / 57.296 } )  }
  sin.angle <- calc(x, fun=function(x) { sin(x) } )
    cos.angle <- calc(x, fun=function(x) { cos(x) } )
      sin.poly <- lapply(extract(sin.angle, y),sum, na.rm=TRUE)
      cos.poly  <- lapply(extract(cos.angle, y),sum, na.rm=TRUE)
    ameans <- rep(NA,length(sin.poly))
      for(i in 1:length(sin.poly)) {
	    if(is.na(sin.poly[[i]]) | is.na(cos.poly[[i]]) ) {
          ameans[i] <- NA
        } else {  		
          ameans[i] <- mang(sin.poly[[i]],cos.poly[[i]])
        }		  
      }	
return(ameans)
}
######################################################		

library(raster)
library(spatialEco)

data(elev)
elev <- raster::projectRaster(elev, crs="+proj=robin +datum=WGS84", res=1000,
                      method='bilinear')
slp <- raster::terrain(elev, opt='slope', unit='radians')
					  
e <- rgeos::gBuffer(as(extent(elev), "SpatialPolygons"), width = -40000) 
  e <- sp::SpatialPolygonsDataFrame(e, data.frame(ID=1), match.ID = FALSE)  
    hex <- spatialEco::hexagons(e, 40000)  
  #hex <- hex[c(3,19),]  
plot(slp)
  plot(hex,add=TRUE)

hex$mslp <- mean.unit.angle(slp, hex, max.angle = 90)

ncols=8	
par(mfrow=c(2,1))
plot(slp)
  plot(hex,add=TRUE)
plot(hex, col=cut(hex$mslp, classBreaks(hex$mslp, ncols), 
     include.lowest = TRUE, labels = 1:ncols) )


