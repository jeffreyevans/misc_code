#' Odds ratio for 2x2 contingency
#' @examples 
#' odds.ratio( cbind(c(56,12), c(20,12)) )
#'
odds.ratio <- function(x, pad.zeros = FALSE, conf.level = 0.95) {
  if (pad.zeros) {
    if (any(x==0)) x <- x + 0.5
   }
  theta <- x[1,1] * x[2,2] / ( x[2,1] * x[1,2] )
    ASE <- sqrt(sum(1/x))
    CI <- exp(log(theta) + c(-1,1) * qnorm(0.5 * (1 + conf.level)) * ASE )
  return( list(estimator=theta, ASE=ASE, conf.interval=CI, 
               conf.level=conf.level) )
}

