###########################################################################
# PROGRAM: BlockStatistics.R
# USE: CALCULATES ZONAL STATISTICS BASED ON NxN BLOCKS                                           
#   BASED ON SOULTION PROVIDED BY WHUBER IN THIS THRED:
#     http://gis.stackexchange.com/questions/34895/create-zonal-grid-in-r   
#                                                                        
# ARGUMENTS:  
#      r       RASTER CLASS OBJECT
#      x       X DIMENSION OF BLOCK IN CELLS
#      y       Y DIMENSION OF BLOCK IN CELLS
#      offset  # DISTANCE OFFSET (DEFAULT IS 0)
#      stat    DESIRED STATISTIC (sum, mean, min, or max) 
#      ...     ADDITIONAL ARGUMENTS PASSED TO zonal                
#                                                                         
# OUTPUT:
#    LIST OBJECT CONTANING 
#      DATA: DATAFRAME WITH ZONE ID (zone) CORESPONDING TO ZONE RASTER AND STATISTIC (stat)
#      ZONES: RASTER OBJECT WITH ZONES                
#     
# EAXMPLE: 
#   require(raster)
#     vgrid <- raster(ncols=1000, nrows=1000, xmn=0)
#       vgrid[] <- runif(ncell(vgrid))
#  ( zm <- blockStatistics(vgrid) )
#   plot(zm$ZONES)	
#   		  
blockStatistics <- function(r, x=100, y=100, offset=0, stat="mean", ...) {
  if (class(r) != "RasterLayer"){ stop("r NEEDS TO BE A raster CLASS OBJECT") }
    zones <- outer(1:nrow(r), 1:ncol(r), function(i,j) (i-(1+offset)) %/% y * ((ncol(r)+1) %/% x) + (j-(1+offset)) %/% x + 1)
      zones <- raster(zones)
        extent(zones) <- extent(r)
   return( zones )
  }
