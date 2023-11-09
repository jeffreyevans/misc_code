# Example of creating a cross-section profile plot
library(spatialEco)
library(terra)
library(sf)
library(dplyr)

elev <- rast(system.file("extdata/elev.tif", package="spatialEco"))

cross <- data.frame(id=1, cbind(c(613160.2, 645160.2),c(4918281, 4930281)), 
                    sequence=c(1,2))
  names(cross)[2:3] <- c("x","y")
down <- data.frame(id=2, cbind(c(658160.2, 671160.2),c(4986281, 4959281)), 
                 sequence=c(1,2))
  names(down)[2:3] <- c("x","y")

xy <- rbind(cross, down)  
  xy <- xy %>%
    st_as_sf(coords = c("x", "y"), crs = st_crs(elev)) %>%
      group_by(id) %>% 
        summarize() %>%
          filter(st_geometry_type(.) == "MULTIPOINT") %>%
            st_cast("LINESTRING")
  
slope <- terrain(elev, "slope", unit="radians")
aspect <- terrain(elev, "aspect", unit="radians")
hill <- shade(slope, aspect, 40, 270)
  plot(hill, col=grey(0:100/100), legend=FALSE, mar=c(2,2,1,4))
    plot(elev, col=rainbow(25, alpha=0.35), add=TRUE, legend=FALSE)
      plot(st_geometry(xy), lwd=2, add=TRUE)

z <- extract(elev, vect(xy))
  z <- lapply(unique(z$ID), \(i) as.numeric(z[z$ID == i,][,2])) 
    names(z) <- c("cross-slope", "down-slope")
	
# Comparison of cross and down slope profiles	
dev.new(height=8, width=16)
  par(mfrow=c(2,1))
lapply(1:length(z), \(i) {
  l = as.integer(st_length(xy[i,]))  
  x <- seq(from=0, to=l, by=as.integer(l/length(z[[i]])))[1:length(z[[i]])]
    plot(x, z[[i]], type="l", xlab="distance", 
         lwd=1.5, ylab="elev", main=paste0(names(z)[[i]], " profile"))
 })

# Influence of smoothing 
dev.new(height=8, width=16)
l = as.integer(st_length(xy[1,]))  
x <- seq(from=0, to=l, by=as.integer(l/length(z[[1]])))[1:length(z[[1]])]
  plot(x, z[[1]], type="l", xlab="distance", 
       lwd=1.5, ylab="elev", main=paste0(names(z)[[1]], " profile"))
    lines(smooth.spline(x, z[[1]], spar = 0.15), lty=1, col="red")
	  lines(smooth.spline(x, z[[1]], spar = 0.4), col="darkgreen")
	    lines(smooth.spline(x, z[[1]], spar = 0.6), col="blue")
    legend("bottomright", legend=c("raw", "s=1", "s=4", "s=6"),
           lty=c(1,1,1,1), col=c("black", "red", "darkgreen", "blue"))


