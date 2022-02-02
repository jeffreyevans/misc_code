#' @title Box-Muller transformation 
#' 
#' @description
#' The Box-Muller transformation. Let u and v be two independent 
#' variables uniformly distributed. Then we can define:
#' x = sqrt(-2*log(u))*sin(2*pi(v))
#' y = sqrt(-2*log(u))*cos(2*pi(v)) 
#'
#' @examples
#' bm <- box_muller(u=runif(100), v=runif(100))
#'   plot(density(c(bm$x, bm$y)))
#' 
#' @export box_muller
box_muller <- function(u, v) {
  n=length(u)
  x=rep(0,n)
  y=rep(0,n)
    for (i in 1:n){
      x[i] <- sqrt(-2*log(u[i]))*cos(2*pi*v[i])
      y[i] <- sqrt(-2*log(u[i]))*sin(2*pi*v[i])
    }
  return( data.frame(x = x, y = y) )
}
