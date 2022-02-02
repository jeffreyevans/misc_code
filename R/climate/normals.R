# (1) Calculations for monthly averages + SD for March, April, May and June for 2005 
#       (so 4 averages + SD for avg temp and precip). If not too much trouble 2nd 
#       calc averages lumping March-June 2005 (not doing separate months).
# (2) Calc Monthly averages + SD for the 4 months March-June  -- aggregating across a 
#       larger time-frame 15-yr (2000-2015). If not too much trouble 2nd calc averages 
#       lumping March-June 2005 (not doing separate months).
# (3) Annual plots showing similar to same plots you emailed showing with all months and 
#       spanning 2000-2015. To show general annual trends.

#library(seas)
# help(package="seas")

setwd("D:/CLIMATE/Jamaica")
sample.season <- c("March", "April", "May","June") 

d <- read.csv("Jamaica_MontegoBay_Daily1995-2016.csv")
  d[d == -9999] <- NA  
  d$date <- as.Date(as.character(d$DATE), "%Y%m%d")
  d <- subset(d, date <= as.Date("2015-12-31") )
  d$month <- factor(format(d$date, "%B"), levels = month.name)
  d$year <- as.numeric(format(d$date, "%Y"))
  d <- d[,7:ncol(d)]

###################################################################
#### Monthly and aggregated mean and sd for March-June 2005 (temp and precp)  
c2005 <- d[d$year == 2005,]
c2005 <- c2005[grep(paste(sample.season, collapse="|"), c2005$month),]
  temp.mean <- mean(c2005$TAVG, na.rm=TRUE)
  temp.sd <- sd(c2005$TAVG, na.rm=TRUE)					
  prcp.mean <- mean(c2005$PRCP, na.rm=TRUE)
  prcp.sd <- sd(c2005$PRCP, na.rm=TRUE) 
    for (m in sample.season){
      csub <- c2005[c2005$month == m,]
	  temp.mean <- append(temp.mean, mean(csub$TAVG, na.rm=TRUE))
	  temp.sd <- append(temp.sd, sd(csub$TAVG, na.rm=TRUE))
	  prcp.mean <- append(prcp.mean, mean(csub$PRCP, na.rm=TRUE))
	  prcp.sd <- append(prcp.sd, sd(csub$PRCP, na.rm=TRUE))
    }
c2005 <- as.data.frame(rbind(temp.mean, temp.sd, prcp.mean, prcp.sd ))	
  names(c2005) <- c("aggregated", sample.season)  
  
###################################################################
#### Monthly and aggregated mean and sd for March-June 2000-2015 (temp and precp)  
c2000.2015 <- d[d$year >= 2000 & d$year <= 2015,]
c2000.2015 <- c2000.2015[grep(paste(sample.season, collapse="|"), c2000.2015$month),]
  temp.mean <- mean(c2000.2015$TAVG, na.rm=TRUE)
  temp.sd <- sd(c2000.2015$TAVG, na.rm=TRUE)					
  prcp.mean <- mean(c2000.2015$PRCP, na.rm=TRUE)
  prcp.sd <- sd(c2000.2015$PRCP, na.rm=TRUE)
    for (m in sample.season){
      csub <- c2000.2015[c2000.2015$month == m,]
	  temp.mean <- append(temp.mean, mean(csub$TAVG, na.rm=TRUE))
	  temp.sd <- append(temp.sd, sd(csub$TAVG, na.rm=TRUE))
	  prcp.mean <- append(prcp.mean, mean(csub$PRCP, na.rm=TRUE))
	  prcp.sd <- append(prcp.sd, sd(csub$PRCP, na.rm=TRUE))
    }  
c2000.2015 <- as.data.frame(rbind(temp.mean, temp.sd, prcp.mean, prcp.sd ))	
  names(c2000.2015) <- c("aggregated", sample.season)  


  
  
d <- d[d$year >= 2000 & d$year <= 2015,]
# {Plot with loess fit
par(mfrow=c(2,2))
  d.precp <- d[-which(is.na(d$PRCP)),] 
  plot(d.precp$date, d.precp$PRCP, type="l", main="precipitation", xlab="", ylab="inches")
    loess.fit <- loess(d.precp$PRCP ~ as.numeric(d.precp$date), span = 0.20)$fitted
      lines(d.precp$date, loess.fit, col = "red", lwd=2)  
  plot(d$date, d$TAVG, type="l", main="mean temperature", xlab="Fahrenheit")
    tmean.fit <- loess(d$TAVG ~ as.numeric(d$date), span = 0.25)$fitted
      lines(d$date, tmean.fit, col = "red", lwd=2)   
  d.tmax <- d[-which(is.na(d$TMAX)),]
  plot(d.tmax$date, d.tmax$TMAX, type="l", main="max temperature", xlab="Fahrenheit")
    tmax.fit <- loess(d.tmax$TMAX ~ as.numeric(d.tmax$date), span = 0.25)$fitted
      lines(d.tmax$date, tmax.fit, col = "red", lwd=2)	  
  d.tmin <- d[-which(is.na(d$TMIN)),]
  plot(d.tmin$date, d.tmin$TMIN, type="l", main="min temperature", xlab="Fahrenheit")
    tmin.fit <- loess(d.tmin$TMIN ~ as.numeric(d.tmin$date), span = 0.25)$fitted
      lines(d.tmin$date, tmin.fit, col = "red", lwd=2) 


c2005 <- d[d$year == 2005,]
c2005 <- c2005[grep(paste(sample.season, collapse="|"), c2005$month),]

d.precp <- d[-which(is.na(d$PRCP)),] 
loess.fit <- loess(d.precp$PRCP ~ as.numeric(d.precp$date), span = 0.20)$fitted
tmean.fit <- loess(d$TAVG ~ as.numeric(d$date), span = 0.25)$fitted
d.tmax <- d[-which(is.na(d$TMAX)),]
tmax.fit <- loess(d.tmax$TMAX ~ as.numeric(d.tmax$date), span = 0.25)$fitted
d.tmin <- d[-which(is.na(d$TMIN)),]
tmin.fit <- loess(d.tmin$TMIN ~ as.numeric(d.tmin$date), span = 0.25)$fitted


tiff( "temp.tif", compression = "lzw", width = 6, height = 6, units = "in", res=600) 	  
  plot(d$date, d$TAVG, ylim=c(min(c(tmin.fit,tmean.fit,tmax.fit)), 
       max(c(tmin.fit,tmean.fit,tmax.fit))), type="l", lwd=2,
       ylab="", xlab="", main="", col="grey")
       lines(lines(d$date, tmean.fit, lwd=2, col="red"))
         lines(lines(d.tmin$date, tmin.fit, lty=3, lwd=1))
         lines(lines(d.tmax$date, tmax.fit, lty=3, lwd=1))
         lines( c2005$date, c2005$TAVG, col="blue" )		 
    legend("bottomright", legend=c("timeseries", "sample period", "trend", "bounds"), 
         lty=c(1,1,1,3), col = c("grey","blue","red","black"), bg="white")
dev.off()

tiff( "precip.tif", compression = "lzw", width = 6, height = 6, units = "in", res=600)	   
  plot(d$date, d$PRCP, type="l", lwd=2, ylab="", xlab="", 
       main="", col="grey")
       lines(lines(d.precp$date, loess.fit, lwd=2, col="red"))
         lines(c2005$date, c2005$PRCP, col="blue")		 		 
    legend("topleft", legend=c("timeseries", "sample period", "trend"), 
         lty=c(1,1,1), col = c("grey","blue","red"), bg="white")		 
dev.off()	
	