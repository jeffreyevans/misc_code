#####################################################################
#####################################################################
# NOAA-NCEI Integrated Surface Hourly Data Base (3505)
# https://www.ncdc.noaa.gov/data-access/land-based-station-data
# Read hourly station data for defined dates 
# Code Version 0.2-0 (04.23.2018)
#####################################################################
#####################################################################
#require(MBA)
#library(fields)
require(rgdal) 
require(spdep) 
require(sp) 
require(RCurl)

########################################################################
########################################################################
# DOWNLOAD *.gz hourlies by weather station and year from NOAA ftp site
########################################################################
########################################################################
setwd("C:/evans/climate/noaa")

download.ncei <- function(download.dir = NULL, min.year=2017, max.year=2017, 
                          create.dir = FALSE, import = TRUE, sp = TRUE, 
                          noaa.ftp = "ftp://ftp.ncdc.noaa.gov/pub/data/noaa") {
  if(is.null(download.dir)) {download.dir = getwd() }
  years=(seq(min.year,max.year,1))
    getContent <- function(dirs) {
        urls <- paste(dirs, "/", sep="")	
          fls <- strsplit(RCurl::getURL(urls, dirlistonly=TRUE), "\r*\n")
          ok <- sapply(fls, length) > 0
        unlist(mapply(paste, urls[ok], fls[ok], sep="", SIMPLIFY=FALSE),
               use.names=FALSE)
      }
    for (y in years) {
	  if(create.dir == TRUE) {
        dir.create(file.path(path, y), showWarnings=FALSE)
          setwd(file.path(path, y))
	}	
    dirs <- paste(noaa.ftp, y, sep="/")
      climate.files <- getContent(dirs)
        cat("\n", "**** downloading data for", y, "****", "\n")
      for ( i in climate.files) {
        cat("downloading", i, "of", length(climate.files), "files", "\n")
          try( download.file(i, destfile=paste(getwd(), unlist(strsplit(i, "/"))[8], sep="/")) )      
      }
    }
	
	
if(import == TRUE) {

column.widths <- c(4,6,5,4,2,2,2,2,1,6,7,5,5,5,4,3,1,1, 
                   4,1,5,1,1,1,6,1,1,1,5,1,5,1,5,1)			   
  for ( y in years ) {
    setwd(file.path(path, y))
      nfiles <- list.files(file.path(path, y))
       stations <- as.data.frame(matrix(NA, length(nfiles),6))
         names(stations) <- c("USAFID", "WBAN", "YR", "LAT","LONG", "ELEV")
      climate.data <- data.frame()
      cat("\n", "**** parsing data for", y, "****", "\n")	  
    for (i in 1:length(nfiles)) {
	  cat("reading", i, "of", length(nfiles), "files", "for", y, "\n")
      cdata <- read.fwf(nfiles[i], column.widths)
        cdata <- cdata[, c(2:8, 10:11, 13, 16, 19, 29,31, 33)]
          names(cdata) <- c("USAFID", "WBAN", "YR", "M","D", "HR", 
  	                      "MIN", "LAT", "LONG", "ELEV", "WIND.DIR", 
  						  "WIND.SPD", "TEMP", "DEW.POINT","ATM.PRES")
          cdata$LAT <- cdata$LAT / 1000
          cdata$LONG <- cdata$LONG / 1000
          cdata$WIND.SPD <- cdata$WIND.SPD / 10
          cdata$TEMP <- cdata$TEMP / 10
          cdata$DEW.POINT <- cdata$DEW.POINT / 10
          cdata$ATM.PRES <- cdata$ATM.PRES / 10
  	climate.data <- rbind(climate.data, cdata)	
      stations[i, 1:3] <- cdata[1, 1:3]
      stations[i, 4:6] <- cdata[1, 8:10]
    }
   } 

# stations data.frame, station ID's and locations.    
# climate.data data.frame, climate measurements for each station
   
# Coerce nodata values to NA's  
climate.data[climate.data == 999] <- NA
  climate.data[climate.data == 999.9] <- NA
  climate.data[climate.data == 9999] <- NA
climate.data[climate.data == 9999.9] <- NA

# SORT DATA 
climate.data <- climate.data[order(climate.data$M, climate.data$D, climate.data$HR), ]
climate.data$DATE <- as.Date(paste(climate.data$YR, climate.data$M, climate.data$D, sep = "-"), 
                             format = "%Y-%m-%d")
							 
# SUBSET TO SINGLE WEATHER STATION
#st <- climate.data[climate.data$USAFID == unique(climate.data$USAFID)[2] ,]
# write.csv(climate.data, paste("WY_Climate" ,"csv",sep="."), row.names=FALSE) 


##################################################
#### Create spatial weather station point locations
na.idx <- row.names(stations[is.na(stations$LAT) | is.na(stations$LONG),])
  if(length(na.idx) > 0) {
    stations <- stations[-match(na.idx, rownames(stations)),]
  }

sids <- paste(stations$USAFID, stations$WBAN, sep=".")  
sp.stations <- SpatialPointsDataFrame(cbind(lon=tapply(stations$LONG,sids,max),
                                            lat=tapply(stations$LAT,sids,max)),
							                data.frame(SIDS=sids))





}	
	
	
	
}
	   
#####################################################################
# IMPORT NOAA WEATHER STATION DATA
#####################################################################

column.widths <- c(4,6,5,4,2,2,2,2,1,6,7,5,5,5,4,3,1,1, 
                   4,1,5,1,1,1,6,1,1,1,5,1,5,1,5,1)			   
  for ( y in years ) {
    setwd(file.path(path, y))
      nfiles <- list.files(file.path(path, y))
       stations <- as.data.frame(matrix(NA, length(nfiles),6))
         names(stations) <- c("USAFID", "WBAN", "YR", "LAT","LONG", "ELEV")
      climate.data <- data.frame()
      cat("\n", "**** parsing data for", y, "****", "\n")	  
    for (i in 1:length(nfiles)) {
	  cat("reading", i, "of", length(nfiles), "files", "for", y, "\n")
      cdata <- read.fwf(nfiles[i], column.widths)
        cdata <- cdata[, c(2:8, 10:11, 13, 16, 19, 29,31, 33)]
          names(cdata) <- c("USAFID", "WBAN", "YR", "M","D", "HR", 
  	                      "MIN", "LAT", "LONG", "ELEV", "WIND.DIR", 
  						  "WIND.SPD", "TEMP", "DEW.POINT","ATM.PRES")
          cdata$LAT <- cdata$LAT / 1000
          cdata$LONG <- cdata$LONG / 1000
          cdata$WIND.SPD <- cdata$WIND.SPD / 10
          cdata$TEMP <- cdata$TEMP / 10
          cdata$DEW.POINT <- cdata$DEW.POINT / 10
          cdata$ATM.PRES <- cdata$ATM.PRES / 10
  	climate.data <- rbind(climate.data, cdata)	
      stations[i, 1:3] <- cdata[1, 1:3]
      stations[i, 4:6] <- cdata[1, 8:10]
    }
   } 

# stations data.frame, station ID's and locations.    
# climate.data data.frame, climate measurements for each station
   
# Coerce nodata values to NA's  
climate.data[climate.data == 999] <- NA
  climate.data[climate.data == 999.9] <- NA
  climate.data[climate.data == 9999] <- NA
climate.data[climate.data == 9999.9] <- NA

# SORT DATA 
climate.data <- climate.data[order(climate.data$M, climate.data$D, climate.data$HR), ]
climate.data$DATE <- as.Date(paste(climate.data$YR, climate.data$M, climate.data$D, sep = "-"), 
                             format = "%Y-%m-%d")
							 
# SUBSET TO SINGLE WEATHER STATION
#st <- climate.data[climate.data$USAFID == unique(climate.data$USAFID)[2] ,]
# write.csv(climate.data, paste("WY_Climate" ,"csv",sep="."), row.names=FALSE) 


##################################################
#### Create spatial weather station point locations
na.idx <- row.names(stations[is.na(stations$LAT) | is.na(stations$LONG),])
  if(length(na.idx) > 0) {
    stations <- stations[-match(na.idx, rownames(stations)),]
  }

sids <- paste(stations$USAFID, stations$WBAN, sep=".")  
sp.stations <- SpatialPointsDataFrame(cbind(lon=tapply(stations$LONG,sids,max),
                                            lat=tapply(stations$LAT,sids,max)),
							                data.frame(SIDS=sids))
plot(sp.stations, pch=19, col="red")


	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
#####################################################################################
# TEMPERATURE TRENDS
#####################################################################################

max.temp <- max(climate.data$TEMP, na.rm=TRUE)
  min.temp <- min(climate.data$TEMP, na.rm=TRUE)
    max.dp <- max(climate.data$DEW.POINT, na.rm=TRUE)
      min.dp <- min(climate.data$DEW.POINT, na.rm=TRUE)

	  
	  
pdf("TemperatureTrends2011.pdf", width=10, height=10)
    for ( i in unique(climate.data$USAFID)) {
      st <- climate.data[climate.data$USAFID == i ,]
        if (!dim(st[st$DEW.POINT == NA ,])[1] / dim(st)[1] <= 0.20) { 	  
        #st <- st[st$MIN == 0, ]
          st <- st[order(st$M, st$D, st$HR), ]
            st$DATE <- as.Date(paste(st$YR, st$M, st$D, sep = "-"),format = "%Y-%m-%d")    							
    # MEAN
    d.mean <- aggregate(st$TEMP, list(DATE = format(st$DATE,"%Y-%m-%d")), mean, na.rm=T)
       d.mean$DATE <- as.Date(d.mean$DATE)
    m.mean <- aggregate(st$TEMP, list(DATE = format(st$DATE,"%Y-%m")), mean, na.rm=T)
      m.mean$DATE <- as.Date(paste(m.mean$DATE, "-15",sep = ""))
    # MIN
    d.min <- aggregate(st$TEMP, list(DATE = format(st$DATE,"%Y-%m-%d")), min, na.rm=T)
      d.min$DATE <- as.Date(d.min$DATE)
    m.min <- aggregate(st$TEMP, list(DATE = format(st$DATE,"%Y-%m")), min, na.rm=T)
      m.min$DATE <- as.Date(paste(m.min$DATE, "-15",sep = ""))    
    # MAX
    d.max <- aggregate(st$TEMP, list(DATE = format(st$DATE,"%Y-%m-%d")), max, na.rm=T)
      d.max$DATE <- as.Date(d.max$DATE)
    m.max <- aggregate(st$TEMP, list(DATE = format(st$DATE,"%Y-%m")), max, na.rm=T)
      m.max$DATE <- as.Date(paste(m.max$DATE, "-15",sep = ""))   
    plot(st$DATE, st$TEMP, main=paste("Temperature", i, sep=" - "), ylab="Temperature (Degrees C)",
         xlab="Month", pch=19, col="lightgrey", ylim=c(min.temp,max.temp))
        points(d.mean$DATE, d.mean$x, col="darkgrey", pch=19)  
            lines(m.min$DATE, m.min$x, type="b", pch=19, col="blue", lty=2, lwd=1.5)
    		lines(m.mean$DATE, m.mean$x, type="b", pch=19, col="black", lty=1, lwd=2)
    	    lines(m.max$DATE, m.max$x, type="b", pch=19, col="red", lty=2, lwd=1.5)
    legend("topleft", c("Monthly Min", "Monthly Mean", "Monthly Max", "Hourly", "Daily mean"), 
           col=c("blue","black", "red", "lightgrey", "darkgrey"), lwd=c(1,1,1,0,0), lty=c(1,1,1,0,0), 
    	   pch=c(19,19,19,19,19) )
      }
  }	  
dev.off()    
       
#####################################################################################
# DEW POINT TRENDS
#####################################################################################
pdf("DewPointTrendsMT2005.pdf", width=10, height=10)
    for ( i in unique(climate.data$USAFID)) {
      st <- climate.data[climate.data$USAFID == i ,] 
	    if (!dim(st[st$DEW.POINT == NA ,])[1] / dim(st)[1] <= 0.20) { 
        #st <- st[st$MIN == 0, ]
          st <- st[order(st$M, st$D, st$HR), ]
            st$DATE <- as.Date(paste(st$YR, st$M, st$D, sep = "-"),format = "%Y-%m-%d")   
# MEAN
d.mean <- aggregate(st$DEW.POINT, list(DATE = format(st$DATE,"%Y-%m-%d")), mean, na.rm=T)
   d.mean$DATE <- as.Date(d.mean$DATE)
m.mean <- aggregate(st$DEW.POINT, list(DATE = format(st$DATE,"%Y-%m")), mean, na.rm=T)
  m.mean$DATE <- as.Date(paste(m.mean$DATE, "-15",sep = ""))

# MIN
d.min <- aggregate(st$DEW.POINT, list(DATE = format(st$DATE,"%Y-%m-%d")), min, na.rm=T)
  d.min$DATE <- as.Date(d.min$DATE)
m.min <- aggregate(st$DEW.POINT, list(DATE = format(st$DATE,"%Y-%m")), min, na.rm=T)
  m.min$DATE <- as.Date(paste(m.min$DATE, "-15",sep = ""))

# MAX
d.max <- aggregate(st$DEW.POINT, list(DATE = format(st$DATE,"%Y-%m-%d")), max, na.rm=T)
  d.max$DATE <- as.Date(d.max$DATE)
m.max <- aggregate(st$DEW.POINT, list(DATE = format(st$DATE,"%Y-%m")), max, na.rm=T)
  m.max$DATE <- as.Date(paste(m.max$DATE, "-15",sep = ""))

plot(st$DATE, st$DEW.POINT, main=paste("Dew Point", i, sep=" - "), ylab="Dew Point",
     xlab="Month", pch=19, col="lightgrey", ylim=c(min.dp,max.dp))
    points(d.mean$DATE, d.mean$x, col="darkgrey", pch=19)  
        lines(m.min$DATE, m.min$x, type="b", pch=19, col="blue", lty=2, lwd=1.5)
		lines(m.mean$DATE, m.mean$x, type="b", pch=19, col="black", lty=1, lwd=2)
	    lines(m.max$DATE, m.max$x, type="b", pch=19, col="red", lty=2, lwd=1.5)
legend("topleft", c("Monthly Min", "Monthly Mean", "Monthly Max", "Hourly", "Daily mean"), 
       col=c("blue","black", "red", "lightgrey", "darkgrey"), lwd=c(1,1,1,0,0), lty=c(1,1,1,0,0), 
	   pch=c(19,19,19,19,19) )
	}   
 }
dev.off() 
