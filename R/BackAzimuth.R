# CALCULATE BACK AZIMUTH
BackAzimuth <- function(d) {
  if(d < 0 | d > 360) stop("OUT OF 0-360 DEGREE RANGE")
   if (d <= 179) { x <- d + 180 } else { x <- abs(d - 180) }
  return(x)
} 
 