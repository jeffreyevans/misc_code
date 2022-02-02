# Field 1: State code (MX for Mexico, CN for Canada)
# Field 2: Station Number
# Field 3: Longitude in decimal degrees
# Field 4: Latitude in decimal degrees
# Field 5: Elevation in meters
# Fields 6-17: Monthly values
# 
# -99.0 is used for a missing value code.
# 
# MinTemp.dat  minimum temps, degrees C
# MeanTemp.dat  average temps, degrees C
# MaxTemp.dat  maximum temps, degrees C
# TotalPrecip.dat  total precipitation, mm.

require(sp)
require(rgdal)
require(raster)
require(rgdal)
require(fields)
setwd("D:/CLIMATE/StationNormals")

col.names <- c("STATE","STATION","LONG","LAT","ELEV",
	           "JAN","FEB","MARCH","APRIL","MAY","JUNE",
			   "JULY","AUG","SEPT","OCT","NOV","DEC")

###########################################################
# MINIMUM TEMPERATURE				
###########################################################
MinTemp <- read.csv("MinTemp.csv")
  names(MinTemp) <- col.names

MinTemp <- MinTemp[MinTemp$STATE != "CN" & MinTemp$STATE != "AK"  
                  & MinTemp$STATE != "CU" & MinTemp$STATE != "GU" 
				  & MinTemp$STATE != "BH" & MinTemp$STATE != "PR" 
				  & MinTemp$STATE != "VI" & MinTemp$STATE != "MX" ,]  
MinTemp$STATE <- factor(MinTemp$STATE)

# COERCE NODATA -999 VALUES TO NA's  
MinTemp[MinTemp == -99] <- NA  
    MinTemp <- na.omit(MinTemp)

MinTemp <- data.frame(MinTemp, 
     MIN=apply(MinTemp[,6:ncol(MinTemp)], MARGIN=1, FUN=min),
     MAX=apply(MinTemp[,6:ncol(MinTemp)], MARGIN=1, FUN=max))
  
# COERCE INTO sp CLASS SpatialPointsDataFrame
coordinates(MinTemp) = ~LONG+LAT
  proj4string(MinTemp) <- "+proj=longlat +datum=WGS84"

ccol=colorRampPalette(c("blue", "yellow", "red"))
  spplot(MinTemp, "MIN", col.regions=ccol(50))    
  spplot(MinTemp, "MIN", col.regions=ccol(50))   
 
writeOGR(MinTemp, getwd(), "MinTemp", driver="ESRI Shapefile",
         check_exists=TRUE, overwrite_layer=TRUE) 
	 
###########################################################
# MAXIMUM TEMPERATURE				
###########################################################
MaxTemp <- read.csv("MaxTemp.csv")
  names(MaxTemp) <- col.names

MaxTemp <- MaxTemp[MaxTemp$STATE != "CN" & MaxTemp$STATE != "AK"  
                  & MaxTemp$STATE != "CU" & MaxTemp$STATE != "GU" 
				  & MaxTemp$STATE != "BH" & MaxTemp$STATE != "PR" 
				  & MaxTemp$STATE != "VI" & MaxTemp$STATE != "MX" ,]  
MaxTemp$STATE <- factor(MaxTemp$STATE)  
  
# COERCE NODATA -999 VALUES TO NA's  
MaxTemp[MaxTemp == -99] <- NA  

MaxTemp <- data.frame(MaxTemp, 
     MIN=apply(MaxTemp[,6:ncol(MaxTemp)], MARGIN=1, FUN=min),
     MAX=apply(MaxTemp[,6:ncol(MaxTemp)], MARGIN=1, FUN=max))
  
# COERCE INTO sp CLASS SpatialPointsDataFrame
coordinates(MaxTemp) = ~LONG+LAT
  proj4string(MaxTemp) <- "+proj=longlat +datum=WGS84"

ccol=colorRampPalette(c("blue", "yellow", "red"))
  spplot(MaxTemp, "MIN", col.regions=ccol(50))    
  spplot(MaxTemp, "MAX", col.regions=ccol(50))    

writeOGR(MaxTemp, getwd(), "MaxTemp", driver="ESRI Shapefile",
         check_exists=TRUE, overwrite_layer=TRUE) 

###########################################################
# AVERAGE TEMPERATURE			
###########################################################
MeanTemp <- read.csv("MeanTemp.csv")
  names(MeanTemp) <- col.names

MeanTemp <- MeanTemp[MeanTemp$STATE != "CN" & MeanTemp$STATE != "AK"  
                    & MeanTemp$STATE != "CU" & MeanTemp$STATE != "GU" 
	     			& MeanTemp$STATE != "BH" & MeanTemp$STATE != "PR" 
				    & MeanTemp$STATE != "VI" & MeanTemp$STATE != "MX" ,]  
MeanTemp$STATE <- factor(MeanTemp$STATE)  
  
# COERCE NODATA -999 VALUES TO NA's  
MeanTemp[MeanTemp == -99] <- NA  

MeanTemp <- data.frame(MeanTemp, 
     MIN=apply(MeanTemp[,6:ncol(MeanTemp)], MARGIN=1, FUN=min),
     MAX=apply(MeanTemp[,6:ncol(MeanTemp)], MARGIN=1, FUN=max))
  
# COERCE INTO sp CLASS SpatialPointsDataFrame
coordinates(MeanTemp) = ~LONG+LAT
  proj4string(MeanTemp) <- "+proj=longlat +datum=WGS84"

ccol=colorRampPalette(c("blue", "yellow", "red"))
  spplot(MeanTemp, "MIN", col.regions=ccol(50))    
  spplot(MeanTemp, "MAX", col.regions=ccol(50))    

writeOGR(MeanTemp, getwd(), "MeanTemp", driver="ESRI Shapefile",
         check_exists=TRUE, overwrite_layer=TRUE) 

###########################################################
# TOTAL PRECIPITATION		
###########################################################
TotalPrecip <- read.csv("TotalPrecip.csv")
  names(TotalPrecip) <- col.names

TotalPrecip <- TotalPrecip[TotalPrecip$STATE != "CN" & TotalPrecip$STATE != "AK"  
                  & TotalPrecip$STATE != "CU" & TotalPrecip$STATE != "GU" 
				  & TotalPrecip$STATE != "BH" & TotalPrecip$STATE != "PR" 
				  & TotalPrecip$STATE != "VI" & TotalPrecip$STATE != "MX" ,]  
TotalPrecip$STATE <- factor(TotalPrecip$STATE)  
  
# COERCE NODATA -999 VALUES TO NA's  
TotalPrecip[TotalPrecip == -99] <- NA  

TotalPrecip <- data.frame(TotalPrecip, 
     MIN=apply(TotalPrecip[,6:ncol(TotalPrecip)], MARGIN=1, FUN=min),
     MAX=apply(TotalPrecip[,6:ncol(TotalPrecip)], MARGIN=1, FUN=max))
  
# COERCE INTO sp CLASS SpatialPointsDataFrame
coordinates(TotalPrecip) = ~LONG+LAT
  proj4string(TotalPrecip) <- "+proj=longlat +datum=WGS84"

ccol=colorRampPalette(c("blue", "yellow", "red"))
  spplot(TotalPrecip, "MIN", col.regions=rev(ccol(50)))    
  spplot(TotalPrecip, "MAX", col.regions=rev(ccol(50)))    

writeOGR(TotalPrecip, getwd(), "TotalPrecip", driver="ESRI Shapefile",
         check_exists=TRUE, overwrite_layer=TRUE) 
		 		 