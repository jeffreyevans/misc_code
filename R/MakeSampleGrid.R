library(raster)
library(sp)
r <- raster(nrows=10, ncols=10, xmn=571823.6, xmx=616763.6, ymn=4423540, 
            ymx=4453690, crs = CRS("+proj=utm +zone=12 +datum=NAD83 
            +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
 r[] <- round(runif(ncell(r)),0) 
    p <- rasterToPolygons(r, fun=function(x){x == 1})[1,]
	

r <- raster(extent(p), nrows=26, ncols=59, crs = CRS("+proj=utm +zone=12 +datum=NAD83 
            +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  r[] <- rep(1,ncell(r))
rp <- as(r, "SpatialPolygonsDataFrame")

plot(rp)
  points(coordinates(rp), pch=19, cex=0.5)

d <- 8  
b <- bbox(p)
xrange <- b[1,][2] - b[1,][1]
yrange <- b[2,][2] - b[2,][1]
  
pts <- SpatialPoints(c(xrange /d , yrange/d)) %>% ceiling()

p.grid <- st_make_grid(st_as_sf(p), square = TRUE, n = c(59,26) ) %>% 
          st_intersection(st_as_sf(p)) %>% 
          st_sf() 
  