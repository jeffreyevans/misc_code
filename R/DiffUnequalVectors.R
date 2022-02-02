# Create dummy example data (note unequal vectors). 
x=runif(20,90,250)
 names(x) <- paste("x",seq(1:length(x)),sep="")
y=runif(10,100,250)
  names(y) <- paste("y", seq(1:length(y))+20, sep="")

# Create dataframe of all combinations and assigns names  
xy <- expand.grid(x,y)
  xy <- data.frame(expand.grid(names(x),names(y)), xy)
    names(xy) <- c("xNames","yNames","x","y")
	
# Calculates absolute difference and returns dataframe of the 
#   maximum value or dataframe of values >= 1 sd. 	
xy$diff <- abs(xy[,"y"] - xy[,"x"])
  xy.max <- xy[which(xy$diff == max(xy$diff)),]
    xy.sd <- xy[which(xy$diff >= sd(xy$diff)),]
########################################################

head(xy)
head(xy.max)
head(xy.sd)	  
