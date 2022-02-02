# REPROJECT, SUBSET AND CONVERT PRISM asc FILE TO img
require(sp)
require(raster)
require(maptools)

setwd("D:/CLIMATE/PRISM")

# REFERENCE RASTER (MASK OF STUDY AREA WITH PROJECTION DEFINED)
ref.r <- raster("D:/PPP_LCC/mask400m.img") 

rlist <- list.files(getwd(), pattern="asc$", full.names=FALSE)
  for(i in 1:length(rlist)) {
    rname <- unlist(strsplit(rlist[i], "[.]"))[1:2]
	  rname <- paste(rname[1], rname[2], sep=".") 
        tr <- readAsciiGrid(rlist[i])
          proj4string(tr) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")	  
            tr <- raster(tr)
              tr <- projectRaster(tr, proj4string(ref.r), method="bilinear")
		        tr <- mask(tr, ref.r)
        writeRaster(tr, filename=paste(rname, "img", sep="."), overwrite=TRUE)				
	}	  
