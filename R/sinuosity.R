#***********************************************************
# Add libraries and internal nc.shp data from sf
#***********************************************************
invisible(lapply(c("sf","nngeo", "spatialEco"), library, 
          character.only = TRUE))

nc <- st_cast(st_read(system.file("shape/nc.shp", package="sf")), "POLYGON")
  nc <- st_transform(nc, 32119) 
    plot(st_geometry(nc))

x <- nc[1,]
  xl <- suppressWarnings(st_cast(x, "LINESTRING"))
  xp <- suppressWarnings(st_cast(x, "POINT"))
plot(st_geometry(xl))
  points(st_coordinates(xp)[,1:2],pch=20,cex=2)

#***********************************************************
# Calculate sinuosity using different scaling methods
#***********************************************************
# sinuosity single polygon
plot(st_geometry(nc[which.max(st_area(nc)),]))
  sinuosity(nc[which.max(st_area(nc)),], "std1")
  sinuosity(nc[which.max(st_area(nc)),], "std2")
  sinuosity(nc[which.max(st_area(nc)),], "length")
  sinuosity(nc[which.max(st_area(nc)),], "raw")

# all polygons sinuosity using no scaling (raw)
nc$sinuosity <- unlist(lapply(1:nrow(nc), function(i) { sinuosity(nc[i,], "raw") } )) 
  plot(nc["sinuosity"], main="Raw sinuosity")  

# all polygons sinuosity using standardized
nc$sinuosity <- unlist(lapply(1:nrow(nc), function(i) { sinuosity(nc[i,]) } )) 
  plot(nc["sinuosity"], main="Polygon Sinuosity")  

# all polygons sinuosity using standardized method 2
nc$sinuosity <- unlist(lapply(1:nrow(nc), function(i) { sinuosity(nc[i,], "std2") } )) 
  plot(nc["sinuosity"], main="Polygon Sinuosity")  
 
# all polygons sinuosity using length
nc$sinuosity <- unlist(lapply(1:nrow(nc), function(i) { sinuosity(nc[i,], "length") } )) 
  plot(nc["sinuosity"], main="Distance weighted sinuosity")  

# simplify vertex count (limit to 1000m spacing of vertices)
ncs <- sf::st_simplify(nc, dTolerance = 1000)
  ncs$sinuosity <- unlist(lapply(1:nrow(ncs), function(i) { sinuosity(ncs[i,], "raw") } )) 
plot(ncs["sinuosity"], main="sinuosity simplified geometry")  


#***********************************************************
# R Function to calculate sinuosity based on convexity
#   requires sf, nngeo libraries 
#   scaled returns standardized, length weighted and unscaled values
#  the degrees argument changes from radians to degrees
#***********************************************************
sinuosity <- function(x, scaled = c("std1", "std2", "length", "raw"), 
                      degrees = FALSE) {
  stopifnot(any(class(x)[1] == c("sf", 
           "SpatialPolygonsDataFrame", 
           "SpatialLinesDataFrame")))
   if(!any(scaled[1] %in% c("std1", "std2", "length", "raw")))
     stop("Not a valid scaling option")
   if(any(class(x)[1] == c("SpatialPolygonsDataFrame", 
           "SpatialLinesDataFrame")))
              x <- sf::st_as_sf(x)           
              stopifnot(sp::is.projected(as(x, "Spatial")))
  scaled = scaled[1] 
  segment <- function (x) {
      geom = sf::st_geometry(x)
      if (class(x)[1] == "sf") 
          dat = sf::st_drop_geometry(x)
      else dat = NULL
       final = list()
      for (i in 1:length(geom)) {
          geom1 = geom[i]
          if (st_is(geom1, "MULTIPOLYGON")) 
              geom1 = sf::st_cast(geom1, "POLYGON")
          line = sf::st_cast(geom1, "LINESTRING")
          result = list()
          for (j in 1:length(line)) {
              pnt = sf::st_cast(line[j], "POINT")
              tmp = list()
              for (k in 1:(length(pnt) - 1)) {
                  tmp[[k]] = pnt[c(k, k + 1)]
              }
              tmp = lapply(tmp, st_combine)
              tmp = lapply(tmp, sf::st_cast, "LINESTRING")
              tmp = do.call(c, tmp)
              result[[j]] = tmp
          }
          result = do.call(c, result)
          if (!is.null(dat)) 
              result = sf::st_sf(result, dat[i, , drop = FALSE])
          final[[i]] = result
      }
      if (!is.null(dat)) {
          final = st_as_sf(data.table::rbindlist(final))
          final = final[1:nrow(final), ]
          class(final) = c("sf", "data.frame")
      }
      else {
          final = do.call(c, final)
      }
      return(final)
  } 
  azimuth <- function (x, y) {
    stopifnot(all(st_is(x, "POINT")))
    stopifnot(all(st_is(y, "POINT")))
    x = st_geometry(x)
    y = st_geometry(y)
    if (length(x) < length(y)) {
        ids = rep(1:length(x), length.out = length(y))
        x = x[ids]
    }
    if (length(y) < length(x)) {
        ids = rep(1:length(y), length.out = length(x))
        y = y[ids]
    }
    x_coords = st_coordinates(x)
    y_coords = st_coordinates(y)
    x1 = x_coords[, 1]
    y1 = x_coords[, 2]
    x2 = y_coords[, 1]
    y2 = y_coords[, 2]
    az = (180/pi) * atan2(x2 - x1, y2 - y1)
    names(az) = NULL
    az[az < 0] = az[az < 0] + 360
    az[x1 == x2 & y1 == y2] = NA
    return(az)
  }
  s <- segment(x)
  a <- unlist(lapply(1:nrow(s), function(i) { 
         x <- sf::st_coordinates(s[i,])[,1:2]
         return(azimuth(sf::st_point(x[1,]), 
		        sf::st_point(x[2,])) * (pi / 180))
        }))
    a <- abs(diff(a))
  if(scaled == "std1") {
    message("Scaling using ", scaled, " argument: ",
	    " sum(a) / (n * pi) " )
	 a <- sum(a / (nrow(s) * pi))
   } else if(scaled == "std2") {
     message("Scaling using ", scaled, " argument: ",
	    " sum( (a * n) / (n * pi) " )
     a <- sum((a * nrow(s)) / (nrow(s) * pi))   
   } else if(scaled == "length") {
     message("Scaling using ", scaled, " argument: ",
	    " sum( (a * n) / (n * pi) * length weights " )
     a <- (a * nrow(s)) / (nrow(s) * pi)
     w <- as.numeric(sf::st_length(s)[-1] / 
                      sum(sf::st_length(s)[-1],na.rm=TRUE))
       a <- sum( a + (a * w) )
   } else if(scaled == "raw") { 
     message("Writing raw values sum(a)")   
     a <- sum(a)    
   } 
  return( a )
}


# Straight-line sinuosity (eg., streams, roads)
line_sinuosity <- function(x) {
  stopifnot(any(class(x)[1] == c("sf", 
           "SpatialPolygonsDataFrame", 
           "SpatialLinesDataFrame")))
   if(any(class(x)[1] == c("SpatialPolygonsDataFrame", 
           "SpatialLinesDataFrame")))
              x <- sf::st_as_sf(x)           
   if(!sp::is.projected(as(x, "Spatial")))
     stop("Data is not projected, cannot be in Latitude/Longitude")
  s <- sf::st_coordinates(x)[,1:2]
    s <- s[-nrow(s),]  
  d <- sqrt( ((s[,1][1] - s[,1][nrow(s)]) ^ 2) +
             ((s[,2][1] - s[,2][nrow(s)]) ^ 2) )  
    d <- d / suppressWarnings(as.numeric(sf::st_length(sf::st_cast(x, "LINESTRING"))) )
  return( d )
}
