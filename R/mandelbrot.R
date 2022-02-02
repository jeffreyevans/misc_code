#' @examples
# Expected Mandelbrot’s D = 1.25
invisible(lapply(c("sf","raster"), library, 
          character.only = TRUE))

#***********************************************************
nc <- st_cast(st_read(system.file("shape/nc.shp", package="sf")), "POLYGON") 
  nc <- st_transform(nc, 32119) 
    plot(st_geometry(nc[which.max(st_area(nc)),]))

d <- seq(100, 10000, 250)
cat("Mandelbrot's Fractal Dimension", mandelbrot(nc[which.max(st_area(nc)),], d)[[1]], "\n")

# Unweighted and area weighted fractal dimension
fd <- unlist(lapply(1:nrow(nc), function(i) { mandelbrot(nc[i,], d)[[1]] } )) 
nc$fd <- fd 
w <- as.numeric(sf::st_area(nc) / sum(sf::st_area(nc)))
  # w <- ((w - max(w)) * -1) + min(w) # inverse weights
nc$afd <-  (fd*100) * w
  plot(nc[c("fd", "afd")])  
 
#***********************************************************
# Import United Kingdom country boundary, project, explode geometry,
#   reproject and subset largest polygon
# getData('ISO3')
prj = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=WGS84 +units=m"
uk <- sf::st_cast(as(raster::getData('GADM', country='GBR', level=0), "sf"), "POLYGON")
  uk <- sf::st_transform(uk, crs=prj) 
    uk <- uk[which.max(st_area(uk)),]

d =  c(25,50,100,150,200,250) 
fd <- mandelbrot(uk, lengths = d*1000)
  cat("Mandelbrot's Fractal Dimension", fd[[1]], "\n") 

# Plot measurement distances
par(mfrow=c(2,3), mai=rep(0,4))
  for (i in 1:length(fd$y)) {
    plot(as(uk, "Spatial"), col='lightgray', lwd=2)
      p <- fd$y[[i]]
      lines(p, col='red', lwd=3)
      points(p, pch=20, col='blue', cex=2)
    }

# fractal (log-log) plot
plot(log(fd$distances/1000), log(fd$n), type='n', xlim=c(2,6), ylim=c(2,6), axes=FALSE,
     xaxs="i",yaxs="i", xlab='Lengths (km)', ylab='Number of segments')
     tics <- c(1,10,25,50,100,200,400)
     axis(1, at=log(tics), labels=tics)
     axis(2, at=log(tics), labels=tics, las=2)
  m <- lm(log(fd$n) ~ log(fd$distances/1000))
    abline(m, lwd=3, col='lightblue')
      points(log(fd$distances/1000), log(fd$n), pch=20, cex=2, col='red')

#' @export
mandelbrot <- function(x, lags, lonlat=FALSE) {
  stopifnot(any(class(x)[1] == c("sf", "SpatialPolygonsDataFrame")))
    stopifnot(nrow(x) == 1)
	g <- st_coordinates(x)[,1:2]
	nr <- nrow(g)
  y <- list()
  for(li in 1:length(lags)) {	
    pts <- 1
    newpt <- 1
    while(TRUE) {
      p <- newpt
        j <- p:(p+nr-1)
          j[j > nr] <- j[j > nr] - nr
          gg <- g[j,]
       #pd <- sp::spDistsN1(gg[-1,], gg[1,], longlat = lonlat)    
       pd <- raster::pointDistance(gg[1,], gg[-1,], lonlat)
     i <- which(pd > lags[li])[1]
        if (is.na(i))
          stop('Measure is longer than the maximum distance found')
        newpt <- i + p
        if (newpt >= nr) break
          pts <- c(pts, newpt)
    }
      pts <- c(pts, 1)
      y[[li]] <- g[pts, ]
  }
      n <- sapply(y, nrow)
    m <- -1 * coefficients(lm(log(n) ~ log(lags)))[2]
  return(list(fractal.dim=round(as.numeric(m),5), y = y, n = n, 
              distances = lags))  
}
