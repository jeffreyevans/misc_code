library(sf)
  nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))

morans.poly(nc, y="BIR79")
morans.poly(nc, y="BIR79", transform="log")


morans.poly <- function(x, y, transform=c("none", "log", "sqrt")) {
  if(!any(class(x)[1] %in% c("sf", "SpatialPolygonsDataFrame")))
    stop("x must be an sf or sp polygon class object")
  if(!y %in% names(x))
    stop("y column does not exist in x")
  if(class(x)[1] == "SpatialPolygonsDataFrame")
    x <- as(x, "sf")	
  n <- nrow(x)
  if(transform[1] == "none") {
    y <- unlist(sf::st_set_geometry(x, NULL)[,y], 
	            use.names = FALSE) 
  } else if(transform[1] == "log") {
    y <- log(unlist(sf::st_set_geometry(x, NULL)[,y], 
	         use.names = FALSE))  
  } else if(transform[1] == "sqrt") {
    y <- sqrt(unlist(sf::st_set_geometry(x, NULL)[,y], 
	          use.names = FALSE))    
  }
  ybar <- mean(y, na.rm=TRUE)
    dy <- y - ybar
      dy_sum <- sum(dy^2, na.rm = TRUE)
        vr <- n/dy_sum
          w <- sf::st_touches(x, sparse = FALSE)
            pm <- tcrossprod(dy)
          pmw <- pm * w
        spmw <- sum(pmw, na.rm = TRUE)
      smw <- sum(w, na.rm = TRUE)
    sw  <- spmw / smw
  return( vr * sw )
}
