#' Create Hilbert space filling curve
#'
#' @examples
#' plot(Hilbert(),type="l")
#' 
#' level <- 6 
#' m <- matrix(0,2^level,2^level)
#' m[Hilbert(level) * 2^level + .5] <- 1:(4^level)
#'  
#' persp(m,theta=35,phi=34,xlab="x",ylab="y",zlab="steps")
#' 
Hilbert <- function(level=5, x=0, y=0, xi=1, xj=0, yi=0, yj=1) {
    if (level <= 0) {
        return(c(x + (xi + yi)/2, y + (xj + yj)/2))
    } else {
        return(rbind(
            Hilbert(level-1, x, y, yi/2, yj/2, xi/2, xj/2),
            Hilbert(level-1, x+xi/2, y+xj/2, xi/2, xj/2, yi/2, yj/2),
            Hilbert(level-1, x+xi/2+yi/2, y+xj/2+yj/2, xi/2, xj/2,  yi/2,  yj/2),
            Hilbert(level-1, x+xi/2+yi, y+xj/2+yj, -yi/2,-yj/2, -xi/2, -xj/2)
        ))
    }
}
 