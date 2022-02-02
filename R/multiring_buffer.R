#' x  sf class feature
#' n  number of rings
#' d  distance(s) between rings
#' ... additional st_buffer args 
#'
#' library(sf)
#' library(raster)
#' 
#' x <- as( extent(4371395, 4372634, 5478811, 5480013), "SpatialPolygons")
#'   proj4string(x) <- "+proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +datum=potsdam +units=m +no_defs" 
#' 
#' ( mrb <- multiring(as(x, "sf"), n=4, d=500, nQuadSegs = 30) )
#' 
#' plot(as(mrb, "Spatial"))
#'   plot(x, col="red", add=TRUE)
#'   
#'
multiring <- function(x, n, d, ...){
  buffers <- list(); names <- list(); nd <- d
    for (i in 1:n){
      buffers[[i]] <- sf::st_as_sf(st_union(st_buffer(x, nd, ...)))
        buffers[[i]]$ID <- paste0("Buffer ", round(nd/1000,1), " km")
      nd <- nd+d
    }
  jlayers <- function(x){ 
    if (length(x)==1){ 
      xm <- x[[1]] 
    } else { 
      for (i in 1:(length(x)-1)){ 
        if(i==1){xm <- x[[1]]} 
        xm <- rbind(xm, x[[i+1]]) 
      } 
    } 
    return(xm) 
  }
  return(jlayers(buffers))
}

