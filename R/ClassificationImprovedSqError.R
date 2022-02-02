#' Classification Improved Squared Error
#' defined as; (1-(P(t|x,0) / v(t)))^2
#' 
#' @examples
#' x=rep(1,100)
#' p=runif(100,0,1)
#'  p[sample(seq(1,100,1), 50)] <- 1
#' cise(x,p) 
#' 
cise <- function(x, p, t=0.90) mean( 1 - ( (x-p)/t )^2 )  


