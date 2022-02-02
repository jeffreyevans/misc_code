#####################################################################
#####################################################################
# DOWNLOAD HOURLY NOAA CLIMATE WEATHERSTATION DATA FROM ftp SITE
#   REQUIRES: gzip DOWNLOAD
#     http://gnuwin32.sourceforge.net/packages/gzip.htm
#####################################################################
#####################################################################
require(RCurl)
path="D:/CLIMATE/NOAA"
gz.path="C:/Program Files (x86)/GnuWin32/bin"
setwd(path)
years=c(2011,2012)

getContent <- function(dirs) {
  if (!require (RCurl)) stop("RCurl PACKAGE MISSING")
    urls <- paste(dirs, "/", sep="")	
      fls <- strsplit(getURL(urls, dirlistonly=TRUE), "\r*\n")
      ok <- sapply(fls, length) > 0
    unlist(mapply(paste, urls[ok], fls[ok], sep="", SIMPLIFY=FALSE),
           use.names=FALSE)
  }
  
# Download *.gz hourly files by weather station, year  
  for (y in years) {
    dir.create(file.path(path, y), showWarnings=FALSE)
      setwd(file.path(path, y))
        dirs <- paste("ftp://ftp.ncdc.noaa.gov/pub/data/noaa", y, sep="/")
      climate.files <- getContent(dirs)
	for ( i in climate.files) {
      try( download.file(i, destfile=paste(getwd(), unlist(strsplit(i, "/"))[8], sep="/")) ) 
        i.name <- unlist(strsplit(i, "/"))	 
          i.name <- i.name[length(i.name)]
      setwd(gz.path)		  
      system(paste("gzip -r -d", paste(file.path(path, y), i.name, sep="/")), 
	         intern=FALSE, ignore.stderr=TRUE, ignore.stdout=TRUE, 
			 show.output.on.console=FALSE) 				  
	    setwd(file.path(path, y))
	  }
  }
  
#####################################################################
# IMPORT NOAA WEATHER STATION DATA
#####################################################################
require(fields)
require(sp)
require(rgdal)

path="D:/CLIMATE/NOAA/2011"
  setwd(path)
 
# DEFINE BINARY DATA STRUCTURE AND CREATE EMPTY DATA 
nfiles <- list.files(path)
  gz.files <- list.files(path, pattern="gz$") 
    nfiles <- nfiles[-which(nfiles %in% gz.files)]
  column.widths <- c(4,6,5,4,2,2,2,2,1,6,7,5,5,5,4,3,1,1, 
                     4,1,5,1,1,1,6,1,1,1,5,1,5,1,5,1)
    climate.data <- data.frame() 
	stations <- as.data.frame(matrix(NA, length(nfiles),6))
      names(stations) <- c("USAFID", "WBAN", "YR", "LAT","LONG", "ELEV")
	  
  for (i in 1:length(nfiles)) {
    cdata <- read.fwf(paste(getwd(), nfiles[i], sep = "/"), column.widths)
      cdata <- cdata[, c(2:8, 10:11, 13, 16, 19, 29,31, 33)]
    names(cdata) <- c("USAFID", "WBAN", "YR", "M","D", "HR", "MIN", "LAT", "LONG", "ELEV",
                      "WIND.DIR", "WIND.SPD", "TEMP", "DEW.POINT","ATM.PRES")
        cdata$LAT <- cdata$LAT/1000
        cdata$LONG <- cdata$LONG/1000
        cdata$WIND.SPD <- cdata$WIND.SPD/10
        cdata$TEMP <- cdata$TEMP/10
        cdata$DEW.POINT <- cdata$DEW.POINT/10
        cdata$ATM.PRES <- cdata$ATM.PRES/10
	climate.data <- rbind(climate.data, cdata)	
    stations[i, 1:3] <- cdata[1, 1:3]
    stations[i, 4:6] <- cdata[1, 8:10]
  }

# COERCE NODATA VALUES TO NA's  
climate.data[climate.data==999] <- NA
  climate.data[climate.data==999.9] <- NA
  climate.data[climate.data==9999] <- NA
climate.data[climate.data==9999.9] <- NA
    
write.csv(climate.data, paste(paste("RawClimate",y,sep=""),"csv",sep="."), row.names=FALSE)  

# COERCE STATION LOCATIONS TO sp CLASS DATA
coordinates(stations) <- ~LONG+LAT
  plot(stations, pch=16, col="red")

  
 
max.temp <- max(climate.data$TEMP, na.rm=T)
min.temp <- min(climate.data$TEMP, na.rm=T)
max.dp <- max(climate.data$DEW.POINT, na.rm=T)
min.dp <- min(climate.data$DEW.POINT, na.rm=T)

# SORT DATA AND COERCE INTO LIST OBJECT
climate.data <- climate.data[order(climate.data$M, climate.data$D, climate.data$HR), ]
climate.data$DATE <- as.Date(paste(climate.data$YR, climate.data$M, climate.data$D, sep = "-"), 
                             format = "%Y-%m-%d")
#climate.list <- split(climate.data, as.factor(climate.data$USAFID))
							 
# SUBSET TO SINGLE WEATHER STATION
#st <- climate.data[climate.data$USAFID == unique(climate.data$USAFID)[2] ,]

#####################################################################################
# TEMPERATURE TRENDS
#####################################################################################
pdf("TemperatureTrendsMT2005.pdf", width=10, height=10)
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

















#####################################################################
# DOWNLOAD NOAA STATION DATA
#####################################################################
file <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/ish-history.csv"
  repeat {
    try(download.file(file, "ish-history.csv", quiet=TRUE))
	  if (file.info("ish-history.csv")$size > 0) { 
	    break
    }
  }
st <- read.csv("ish-history.csv")
names(st)[c(3, 10)] <- c("NAME", "ELEV")
  st <- st[, -5]
  st <- st[st$CTRY == "US", ]
    st$LAT <- st$LAT/1000
     st$LON <- st$LON/1000
     st$ELEV <- st$ELEV/10
     st$BEGIN <- as.numeric(substr(st$BEGIN, 1, 4))
     st$END <- as.numeric(substr(st$END, 1, 4))

# RANGE OF START AND END YEARS
range(st$BEGIN, na.rm=T)
range(st$END, na.rm=T)	 

#####################################################################	 
# CREATE A STATE AND TIME-RANGE QUERY
#####################################################################	 
# FOR MULTIPLE YEARS
#mi.list <- st[st$STATE == "WY" & (st$BEGIN <= 2000 & st$END >= 2012 | is.na(st$BEGIN)), ]
#dim(mi.list)

# FOR SINGLE YEAR
mi.list <- st[st$STATE == "WY" & (st$BEGIN <= 2005 & st$END >= 2005 & !is.na(st$BEGIN)), ]
dim(mi.list)	