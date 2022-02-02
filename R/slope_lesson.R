# Cell index for the 3x3 window
# Z1    Z2    Z3
# Z4    Z5    Z6
# Z7    Z8    Z9
#
# Vector bracket indexing
# z1=m[1], z2=m[2], z3=m[3], z4=m[4], z5=m[5]
# z6=m[6], z7=m[7], z8=m[8], z9=m[9]
# m is a vector n=9 representing the values of a 3x3 window 
# 
#  Zevenbergen, L. W. & Thorne, C. R. (1987). Quantitative Analysis of Land 
#    Surface Topography. Earth Surface Processes and Landforms. 12:47-56.
#
# radians ratio function sin(x)/x
###############################################################


###############################################################
# Radians to degrees with degree range constraint 
#   Uses the deg constant based on 1 radian = 57.295 degrees
#   For conversion to slope degrees lims=90 and for aspect lims=180
radians.degrees <- function(x, lims = 180) { ((x * (lims/pi))/0.572957795786) * 0.01 }
 
###############################################################
# Zevenbergen and Thorne (1987) slope
zevenbergen.slope <- function(m, res=1000, radians = FALSE, ...){
  if( length(m) > 9) stop("Windows larger than 3x3 are not supported")
    p <- (m[6] - m[4]) / (2 * res)
      q <- (m[2] - m[8]) / (2 * res)
    slope <- atan(sqrt(p^2 + q^2))
  if(radians == TRUE) {
    return(slope / 0.572957795786) * 0.01
  } else {  
    return(slope)
  }
}

###############################################################
###############################################################
# Example
###############################################################
###############################################################				 
library(raster)
library(spatialEco)
  data(elev)
  elev <- projectRaster(elev, crs="+proj=robin +datum=WGS84", res=1000,
                        method='bilinear')
					  
# Calculate slope in radians
( slp.rad <- focal(elev, w=matrix(1,nrow=3,ncol=3),   
                 fun = zevenbergen.slope, 
                 pad = TRUE, padValue = 0) )

# Calculate slope in degrees and percent. 
#   Note; resource function with radians = FALSE  	 
( slp.deg <- focal(elev, w=matrix(1,nrow=3,ncol=3),   
                 fun = zevenbergen.slope, 
                 pad = TRUE, padValue = 0) )
( slp.pct <- calc(slp.deg, fun=function(x) { tan(x) * 100 } ) )

par(mfrow=c(2,2))
  plot(elev, main="Elevation")
  plot(slp.deg, main="slope (degrees)")
  plot(slp.pct, main="slope (percent)")
  plot(slp.rad, main="slope (radians)")

# Calculate the ratio of sin of radians			 
rad.sin <- spatialEco::raster.invert(calc(slp.rad, 
                   fun=function(x) { sin(x)/x } )) * 100
rad.sin <- ( rad.sin - min(values(rad.sin),na.rm=TRUE) ) / 
           ( max(values(rad.sin),na.rm=TRUE) - min(values(rad.sin),na.rm=TRUE) )
			

# Shows that it is scalable as a distribution moment per unit area
rad.mean <- focal(rad.sin, w=matrix(1,nrow=5,ncol=5), fun=mean) 

# Now lets plot it
par(mfrow=c(2,2))
  plot(elev, main="Elevation")
  plot(slp.rad, main="slope (radians)")
  plot(rad.sin, main="sine(F(x,y)) ratio")
  plot(rad.mean, main="Focal mean (5x5) sine(F(x,y)) ratio")
  
#### Create some polygons  
e <- rgeos::gBuffer(as(extent(slp.rad), "SpatialPolygons"), width = -40000) 
  e <- SpatialPolygonsDataFrame(e, data.frame(ID=1), match.ID = FALSE)  
    hex <- spatialEco::hexagons(e, 40000)  
  hex <- hex[c(3,19),]  
 
plot(slp.rad)
  plot(hex,add=TRUE)

v <- extract(slp.rad, hex)

mean.slope <- function(x) {
  x[x <= 0] <- 0.01745329
  slp.arctan = atan2(sin(x), cos(x))
  return( mean((360 + slp.arctan * (180 / pi)) %% 360, na.rm=TRUE) )
}
 
( slp.means <- lapply(v, mean.slope) )

 
# Calulate cos/sin
slp.rad[slp.rad <= 0] <- 0.01745329
cos.spl = cos(slp.rad)
sin.slp = sin(slp.rad)
 
# atan of sine and cosine
slp.arc.tan = atan2(sin.slp, cos.spl)

# Convert back to slope units 
slp.mean = (360 + slp.arc.tan * (180 / pi)) %% 360

  
  
  
  
  