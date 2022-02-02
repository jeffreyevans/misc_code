#' @title Kriging log back-transform
#' @description back-transform of log-transformed Kriging predictions and variances 
#'
#' @param y          Log transformed kriging predictions
#' @param x          Variances of log transformed kriging predictions 
#' @param type       c("both", "y", "var") back-transformation variable 
#' @param mean.corr  FALSE/TRUE Apply mean-bias correction (Yammamoto 2007)  
#' 
#' @return vector or data.frame of back transformed y, variance or both
#'
#' @note
#' when back-transformed lognormal kriging estimates are biased, not equal to the sample mean, the mean.corr 
#'   can be used following Yammamoto (2007). Bias correction is done by multiplying Laurent's (1963) bias correction
#'   by a correction factor representing the ratio of the sample mean to the back-transformed means. 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#' 
#' @references Laurent, A. G. (1963) The Lognormal Distribution and the Translation Method: Description and Estimation Problems
#'  Journal of the American Statistical Association 58(301):231-235
#' @references Yammamoto, J.K., (2007) On unbiased backtransform of lognormal kriging estimates, 
#'  Computational Geosciences 11(3):219-234
#'
#' @examples
#'   library(gstat)
#'   library(sp)
#'   
#'   data(meuse)
#'   coordinates(meuse) = ~x+y
#'   data(meuse.grid)
#'   
#'   #fit variogram with log transform of zinc and predict
#'   v <- variogram(log(zinc)~1, data=meuse, meuse.grid)
#'   m <- vgm(0.59,"Sph", 897, 0.05)
#'   gridded(meuse.grid) = ~x+y
#'   a <- krige(log(zinc)~1, meuse, meuse.grid, m)
#'   
#'   # Back transform log predictions and variances
#'   a@data <- data.frame(a@data, krige.back.transform(y=a$var1.pred, 
#'                        x=a$var1.var, type="both", 
#'   					  mean.corr = TRUE))
#'   
#'   # Compare to exp back-transform
#'   par(mfrow=c(3,1))
#'     plot(density(meuse$zinc), main="raw y")
#'     plot(density(exp(a$var1.pred)), main="exp back-transform")  
#'     plot(density(a$pred), main="bais corrected back-transform") 
#' 
#' @export
krige.back.transform <- function(x, y, type="both", mean.corr = FALSE) { 
  if(type == "y" | type == "both") {
    # Back transform log(y) 
	bt.y <- exp( y + (x / 2) )
      if(mean.corr) {
        if(mean(bt.y) == mean(y)) 
          message("Correction not needed")		
	  } else {
        bt.y <- bt.y * ( mean(y) / mean(bt.y) ) 
      }	  
  }
  if(type == "var" | type == "both") {
    # Back transform prediction variances of log(y) 
    bt.var <- exp(2 * y + x) * (exp(x) - 1) 
  }
  if(type == "both") {  
    return(data.frame(pred = bt.y, var = bt.var))
  } else if(type == "y") {
    return(bt.y)  
  } else if(type == "var") {
    return(bt.var)
  } else {
    stop("Not a valid type")
  }	
}
