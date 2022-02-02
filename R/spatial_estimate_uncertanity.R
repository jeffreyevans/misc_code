ibrary(ranger)
library(rgdal)
library(raster)
library(GSIF)

demo(meuse, echo=FALSE)
meuse.grid$SW_occurrence = readGDAL("data/meuse/Meuse_GlobalSurfaceWater_occurrence.tif")$band1[meuse.grid@grid.index] ## flooding occurrence
meuse.grid$AHN = readGDAL("data/meuse/ahn.asc")$band1[meuse.grid@grid.index] ## AHN.nl precise elevation
meuse.grid$LGN5 = as.factor(readGDAL("data/meuse/lgn5.asc")$band1[meuse.grid@grid.index]) ## land use class

## convert to indicators:
meuse.grid@data = cbind(meuse.grid@data, data.frame(model.matrix(~LGN5-1, meuse.grid@data)))

## Binomial variable Prediction error ----
meuse@data = cbind(meuse@data, data.frame(model.matrix(~soil-1, meuse@data)))
summary(as.factor(meuse$soil1))
fm.s1 = as.formula(paste("soil1 ~ ", paste(names(grid.dist0), collapse="+"), " + SW_occurrence + dist"))
rm.s1 <- do.call(cbind, list(meuse@data["soil1"], 
                 over(meuse["soil1"], meuse.grid), 
				 over(meuse["soil1"], grid.dist0)))

## Option 1: treat binomial variable as numeric variable
m1.s1 <- ranger(fm.s1, rm.s1, mtry=22, num.trees=500, seed = 1, quantreg=TRUE)
fm.s1c <- as.formula(paste("soil1c ~ ", paste(names(grid.dist0), collapse="+"), " + SW_occurrence + dist"))
rm.s1$soil1c = as.factor(rm.s1$soil1)

## Option 2: treat binomial variable as factor variable
m2.s1 <- ranger(fm.s1c, rm.s1, mtry=22, num.trees=500, seed = 1, probability=TRUE, keep.inbag = TRUE)

## Conclusion: OOB MSE is about the same as the OOB prediction error
## Derive prediction errors:
pred.soil1_rfb = predict(m1.s1, cbind(meuse.grid@data, grid.dist0@data), type="quantiles", quantiles=quantiles)
meuse.grid$soil1_rfq_U = pred.soil1_rfb$predictions[,3]
meuse.grid$soil1_rfq = pred.soil1_rfb$predictions[,2]
meuse.grid$soil1_rfq_L = pred.soil1_rfb$predictions[,1]

## Assuming normal distribution of errors this should match 1 s.d. of the prediction error:
meuse.grid$soil1_rfq_r = (meuse.grid$soil1_rfq_U - meuse.grid$soil1_rfq_L)/2
mean(meuse.grid$soil1_rfq_r, na.rm=TRUE); sqrt(m1.s1$prediction.error)

## Again, quantreg error smaller than the OOB RMSE
pred.soil1_rfc = predict(m2.s1, cbind(meuse.grid@data, grid.dist0@data), type="se")
meuse.grid$soil1_rfc = pred.soil1_rfc$predictions[,2]
mean(pred.soil1_rfc$se[,2], na.rm=TRUE); sqrt(m2.s1$prediction.error)

## Again much smaller and needs to be scaled:
meuse.grid$soil1_rfc_r = pred.soil1_rfc$se[,2] * sqrt(m2.s1$prediction.error)/mean(pred.soil1_rfc$se[,2], na.rm=TRUE)







## Some examples with deriving uncertainty (for spatial prediction) using the ranger package
## tom.hengl@isric.org

list.of.packages = c("GSIF", "plotKML", "entropy", "plyr", "parallel", "ranger", 
                     "raster", "doParallel", "doMC", "rgdal", "caret", "dismo", 
					 "snowfall")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(plotKML) # has the eberg data
library(GSIF)
library(entropy) #not needed
library(ranger)
library(rgdal)
library(dismo)
library(raster)
library(caret)
library(parallel)
library(doParallel)
library(doMC)
library(plyr)
library(snowfall)
cpus = 8
registerDoMC(cpus)

# Build example dataset(s)
data(eberg)
  coordinates(eberg) <- ~X+Y
  proj4string(eberg) <- CRS("+init=epsg:31467")

data(eberg_grid)
  gridded(eberg_grid) <- ~x+y
  proj4string(eberg_grid) <- CRS("+init=epsg:31467")

eberg_spc <- GSIF::spc(eberg_grid, ~ PRMGEO6 + DEMSRT6 + TWISRT6 + TIRAST6)
  eberg_grid@data <- cbind(eberg_grid@data, eberg_spc@predicted@data)

# overlay and create a regression-matrix:
ov <- over(eberg, eberg_grid)
  m <- cbind(ov, eberg@data)

#####################################
# Uncertainty nominal response 
# variable
#####################################

## clean-up target variable:
xg = summary(m$TAXGRSC, maxsum=length(levels(m$TAXGRSC)))
  str(xg)
    selg.levs = attr(xg, "names")[xg > 5]
m$soiltype <- m$TAXGRSC
  m$soiltype[which(!m$TAXGRSC %in% selg.levs)] <- NA
    m$soiltype <- droplevels(m$soiltype)
      m$soiltype <- as.factor(m$soiltype)

## fit model:
fm1 = as.formula(paste("soiltype ~", paste0("PC", 1:10, collapse = "+")))
  mD = m[complete.cases(m[,all.vars(fm1)]),all.vars(fm1)]
( TAXGRSC.rf <- ranger(fm1, data = mD, write.forest = TRUE, 
                       probability = TRUE) )

# Entropy function
entropy.empirical <- function(y){
  entropy <- function(freqs){
    freqs <- freqs / sum(freqs)
    H = -sum( ifelse(freqs > 0, freqs*log(freqs), 0) )
    return(H)
  }
  return( entropy(y / sum(y)) )
}

## predict values:
eberg_soiltype = eberg_grid[1]
  eberg_soiltype@data <- data.frame(predict(TAXGRSC.rf, eberg_grid@data, 
                                    na.action = na.pass)$predictions)
  str(eberg_soiltype@data)

## uncertainty can be represented using the Shannon Entropy (entropy package); 
##   some examples:
entropy.empirical(c(0,0,0,0,1)) ## smallest entropy
entropy.empirical(c(1/5,1/5,1/5,1/5,1/5)) ## highest entropy

## scaled entropy using log-base as number of classes:
p=c(0,0.8,0.2,0,0)
#sum(-p*log(p,base=length(p)), na.rm=TRUE)
entropy.empirical(p) / entropy.empirical(rep(1 / length(p),length(p)))

## same result
entropy.empirical(unlist(eberg_soiltype@data[1,])) ## example

## apply entropy function, in parallel
eberg_soiltype$SE = unlist(alply(eberg_soiltype@data, 1, 
                          .fun=function(x){entropy.empirical(unlist(x))}, 
						  .parallel = TRUE))
hist(eberg_soiltype$SE, col="grey")

## divide by maximum possible uncertainty:
eberg_soiltype$SE = round(eberg_soiltype$SE / 
                    entropy.empirical(rep(1 / length(levels(m$soiltype)),
					length(levels(m$soiltype))))*100)

## plot:
plot(raster(eberg_soiltype["SE"]), col=SAGA_pal[[1]])
  points(eberg[!is.na(eberg$TAXGRSC),], pch="+")
#**** high uncertainty correlates with extrapolation areas

#####################################
# Uncertainty for numeric response 
# variables
#####################################
fm2 = as.formula(paste("SNDMHT_D ~", paste0("PC", 1:10, collapse = "+")))
  mS = m[complete.cases(m[,all.vars(fm2)]),all.vars(fm2)]
( SNDMHT.rf <- ranger(fm2, data = mS, write.forest = TRUE) )
  sqrt(SNDMHT.rf$prediction.error) ## mean error = +/-15

# Estimate CV residuals with re-fitting ("repeated CV"):
predict_cv_resid = function(formulaString, data, nfold){
  sel <- dismo::kfold(data, k=nfold)
  out = list(NULL)
    for(j in 1:nfold){
      s.train <- data[!sel==j,]
      s.test <- data[sel==j,]
      m <- ranger(formulaString, data=s.train, write.forest=TRUE)
      pred <- predict(m, s.test, na.action = na.pass)$predictions
      obs.pred <- as.data.frame(list(s.test[,all.vars(formulaString)[1]], pred))
      names(obs.pred) = c("Observed", "Predicted")
      obs.pred[,"ID"] <- row.names(s.test)
      obs.pred$fold = j
      out[[j]] = obs.pred
    }
      out <- plyr::rbind.fill(out)
    out <- out[order(as.numeric(out$ID)),]
  return(out)
}

resid.SNDMHT = predict_cv_resid(fm2, data=mS, nfold=5)
  xyplot(Predicted~Observed, resid.SNDMHT, asp=1, 
         par.settings=list(plot.symbol = list(col=alpha("black", 0.6), 
		 fill=alpha("red", 0.6), pch=21, cex=0.9)), xlab="measured", 
		 ylab="predicted (ranger)")
mS$resid = abs(resid.SNDMHT$Observed - resid.SNDMHT$Predicted)

## model absolute residuals as function of covariates:
var.fm2 = as.formula(paste("resid ~", paste0("PC", 1:10, collapse = "+")))
( var.SNDMHT.rf <- ranger(var.fm2, data = mS, write.forest = TRUE) )

## predict uncertainty:
eberg_SNDMHT = eberg_grid[1]
  eberg_SNDMHT$SNDMHT_D <- predict(SNDMHT.rf, eberg_grid@data, na.action = na.pass)
    plot(raster(eberg_SNDMHT["SNDMHT_D"]), col=SAGA_pal[[1]])

## prediction error:
eberg_SNDMHT$var.SNDMHT_D <- predict(var.SNDMHT.rf, eberg_grid@data, 
                                     na.action = na.pass)$predictions
  summary(eberg_SNDMHT$var.SNDMHT_D)

## mean/median is a bit smaller than what we get with ranger OOB - probably because in most of the study area SND content is low
eberg_SNDMHT$var.SNDMHT_Df = ifelse(eberg_SNDMHT$var.SNDMHT_D>20, 20, 
                                    eberg_SNDMHT$var.SNDMHT_D)

  plot(raster(eberg_SNDMHT["var.SNDMHT_Df"]), col=SAGA_pal[[1]])
    points(eberg[!is.na(eberg$SNDMHT_D),], pch="+")
## uncertainty is function of location / covariates, but also of values (higher values - higher uncertainty usually):

#################################################
# Since ranger v0.7.2 errors can be extracted 
# directly from the package based on the method 
# of Wager et al. (2014) 
## https://github.com/imbs-hl/ranger/issues/136
SNDMHT.rfu <- ranger(fm2, data = mS, num.trees = 85, write.forest = TRUE, 
                     keep.inbag = TRUE)
  eberg_SNDMHT = eberg_grid[1]

## this is somewhat computationally intensive if the number of trees is high!
x = predict(SNDMHT.rfu, eberg_grid@data, type = "se") 
  eberg_SNDMHT$SNDMHT_D <- x$predictions
    # standard errors (absolute errors +/-)
    eberg_SNDMHT$sd.SNDMHT_D <- x$se 


pi_upper = m + 1.96*se
pi_lower = m - 1.96*se

#**** in some areas errors are quite high!
plot(raster(eberg_SNDMHT["SNDMHT_D"]), col=SAGA_pal[[1]])
  plot(raster(eberg_SNDMHT["sd.SNDMHT_D"]), col=SAGA_pal[[1]]) 
    points(eberg[!is.na(eberg$SNDMHT_D),], pch="+")

## compare numbers:
summary(x$se)
  SNDMHT.rfu
  
  
  
  

#**** median se about 15 hence good match!
