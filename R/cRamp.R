##########################################################################
# PROGRAM: cRamp
# USE: CREATE A COLORRAMP OF A SPECIFIED VECTOR
#           
# ARGUMENTS: 
#       x      A NUMERIC OR FACTORAL VECTOR OF VALUES 
#       d      RECONIZED R COLOR NAMES TO RAMP BETWEEN
#
# VALUE:
#     A VECTOR OF COLORNAMES THAT CORRESPONDS TO THE ORDER OF x
#
# EXAMPLES: 
#      x <- runif(100)
#      y <- runif(100)
#
#  # PLOT WITH FACTORAL VARIABLE
#      z <- gl(5, 10, length=length(x))
#      plot(x,y,col=cRamp(z),pch=19,cex=1.25)
#      plot(x,y,col=cRamp(z,d=c("red","green","blue")),pch=19,cex=1.25)
#
#  # PLOT WITH CONTINIOUS VARIABLE
#      z <- y*x
#      plot(x,y,col=cRamp(z),pch=19,cex=1.25)
#    	                               
# CONTACT: 
#     Jeffrey S. Evans 
#     Senior Landscape Ecologist 
#     The Nature Conservancy - Central Science
#     Adjunct Assistant Professor
#     Zoology and Physiology
#     University of Wyoming
#     Laramie, WY
#     (970)672-6766
#     jeffrey_evans@tnc.org
##########################################################################
cRamp <- function(x,d=c("blue", "red")){
  crange <- function(x)(as.numeric(x)-min(as.numeric(x)))/diff(range(as.numeric(x)))
    cols <- colorRamp(d)(crange(x))
  apply(cols, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
}  
