######################################################################################
# PROGRAM: WtCentroid 
# USE: CALCULATES WEIGHTED COORDINATES CENTROID  
# REQUIRES: sp 
#
# ARGUMENTS:  
#       x      sp SpatialPointsDataFrame CLASS OBJECT                       
#       p      WEIGHTS FEILD 
#       sp     OUTPUT sp SpatailPoints CLASS OBJECT (DEFAULT TRUE)    
# 
# VALUE:  
#      A VECTOR OF COORDINATE CENTROID(S) OR sp CLASS POINT OBJECT         
#
# NOTES:
#      [Xw]=[X]*[p], [Yw]=[Y]*[p]
#      [sXw]=SUM[Xw], [sYw]=SUM[Yw], [sP]=SUM[p].
#      wX=[sXw]/[sP], wY=[sYw]/[sP]
#      
#      WHERE; 
#        X=X COORDINATE(S)
#        Y=Y COORDINATE(S)
#        p=WEIGHT 
#
# EXAMPLES: 
#    require(sp)
#    data(meuse)
#    coordinates(meuse) = ~x+y
#    WtXY <- wtCentroid(meuse, "copper", sp=TRUE)  
#      plot(meuse)
#        points(WtXY, pch=19, col="red")    
#                                                                                       
# CONTACT:
#     Jeffrey S. Evans
#     Senior Landscape Ecologist  
#     The Nature Conservancy
#     Central Science/DbyD
#     Affiliate Assistant Professor
#     Environment and Natural Resources
#     University of Wyoming
#     Laramie, WY 82070 
#     jeffrey_evans@tnc.org
#     (970) 672-6766
######################################################################################
wtCentroid <- function (x, p, sp=TRUE) 
  {
    if (!require (sp)) stop("sp PACKAGE MISSING")
      if (!inherits(x, "SpatialPointsDataFrame")) 
    stop(deparse(substitute(x)), " MUST BE A SpatialPointsDataFrame OBJECT")
	p=x@data[,p]
     Xw=sum(coordinates(x)[,1] * p) 
      Yw=sum(coordinates(x)[,2] * p)
    wX=Xw/sum(p)    
	wY=Yw/sum(p)
	if(sp == FALSE) { 
	  return(c(wX, wY))
	 } else {
	  xy <- SpatialPoints(matrix(c(wX, wY), nrow=1, ncol=2))
        proj4string(xy) <- proj4string(x)	  
	  return(xy)
	}
  }
