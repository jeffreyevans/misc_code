###########################################################################
# PROGRAM: PctImageDiff.R
# USE: COMPUTES THE DIFFERENCES BETWEEN TWO RASTERS AND HIGHLIGHTS CHANGE 
#        THAT EXCEEDS A +/- PERCENT THRESHOLD (p)                                          
#                                                                         
# ARGUMENTS:
#      r1           FIRST DATE IMAGE - OBJECT OF CLASS raster  
#      r2           SECOND DATE IMAGE - OBJECT OF CLASS raster 
#      p            PERCENT CHANGE THRESHOLD
#      filename     OUTPUT RASTER FILE (DEFAULT=FALSE)
#      ...          OPTIONAL ARGUMENTS PASSED TO calc, writeRaster OR dataType              
#                                                                         
# EAXMPLE: 
#     require(raster)
#     r2002 <- raster(ncols=100, nrows=100)
#       r2002[] <- round(runif(ncell(r2002), 1,10), digits=0)
#     r2011 <- raster(ncols=100, nrows=100)
#       r2011[] <- round(runif(ncell(r2011), 1,10), digits=0)
#     rchange <- PctImageDiff(r1=r2002, r2=r2011, p=10)
#       arg <- list(at=c(1,2,3,4,5), labels=c("High Decrease","Decrease","Unchanged", 
#                   "Increase", "High Increase"))
#     	 cols=c("cyan", "blue", "white", "yellow", "red")
#     plot(rchange, col=cols, axis.args=arg, main="Change")
#		  
# OUTPUT: 
#     raster CLASS OBJECT OR RASTER FILE TO DISK                
#
# NOTES:
#     1=High Decrease, 2=Decrease, 3=Unchanged, 4=Increase,
#     5=High Increase  
#
# CONTACT: 
#   Jeffrey S. Evans
#   Senior Landscape Ecologist  
#   The Nature Conservancy
#   Central Science/Conservation Lands 
#   Adjunct Faculty
#   Environment and Natural Resources
#   University of Wyoming
#   Laramie, WY 82070 
#   jeffrey_evans@tnc.org
#   (970) 672-6766 
###########################################################################
PctImageDiff <- function(r1, r2, p=10, filename=FALSE, ...) {
  rdiff = r2 - r1
    dmin=cellStats(rdiff, stat='min', na.rm=TRUE)
      dmax=cellStats(rdiff, stat='max', na.rm=TRUE)
        dpct <- min(c(0.0, dmin)) * min( max(c( 0.0, p)), 100) / 100.0
          ipct <- max(c(0.0, dmax)) * min( max(c( 0.0, p)), 100) / 100.0 
    rclass <- function(x) { 
      ifelse( x < dpct, 1, 
        ifelse( x >= dpct & x < 0, 2,
          ifelse( x == 0, 3,
            ifelse( x > 0 & x <= ipct, 4,
              ifelse( x > ipct, 5, NA ))))) }
    if (filename != FALSE) {
	  calc(rdiff, fun=rclass, filename=filename, ...)
	    print(paste("RASTER WRITTEN TO", filename, sep=": "))
       } else {
      return(calc(rdiff, fun=rclass))
  }
} 
 