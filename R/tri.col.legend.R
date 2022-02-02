#### Creates a three-color triangle color gradient
# Function for calculating the color of a set of points `pt`
# in relation to the triangle
tricol <- function(pt, sharpness=2){
  tri <- rbind(sin(0:2*2/3*pi), cos(0:2*2/3*pi))
    RGB <- sapply(1:3, function(i){
                  a <- sweep(pt, 2, tri[,i])
                  b <- apply(tri[,-i], 1, mean) - tri[,i]
                  sharpness * ((a %*% b) / sum(b^2)) - sharpness + 1} )
    RGB[-splancs::inpip(pt,t(tri)),] <- 1    # Color points outside the triangle white
  do.call(rgb, unname(as.data.frame(pmin(pmax(RGB, 0), 1))))
}

# Plot
res <- 1000  # Resolution

# Axis points
xi <- seq(-1, 1, length=res)        
yi <- seq(-.8, 1.2, length=res)

# Midpoints between axis points
x <- xi[1] + cumsum(diff(xi))       
y <- yi[1] + cumsum(diff(yi))

xy <- matrix(1:(length(x)*length(y)), length(x))
  image(xi, yi, xy, col=tricol(as.matrix(expand.grid(x,y))), 
      useRaster=TRUE, axes =FALSE, ylab="", xlab="")  
# tri <- rbind(sin(0:2*2/3*pi), cos(0:2*2/3*pi))	  
#   lines(tri[1,c(1:3,1)], tri[2,c(1:3,1)], type="l")


#############################################################
# plot the Maxwell triangle using Ternary
library(Ternary)
Ternary::TernaryPlot(alab = "Red \u2192", 
                     blab = "\u2190 Green", 
					 clab = "Blue \u2192",
                lab.col = c('red', 'darkgreen', 'blue'),
                point = 'up', lab.cex = 0.8, grid.minor.lines = 0,
                grid.lty = 'solid', col = rgb(0.9, 0.9, 0.9), grid.col = 'white', 
                axis.col = rgb(0.6, 0.6, 0.6), ticks.col = rgb(0.6, 0.6, 0.6),
                axis.rotate = FALSE,
                padding = 0.08, 
			    axis.labels = seq(0,1,0.1))
    Ternary::ColourTernary(Ternary::TernaryPointValues(rgb), 
	                       spectrum = NULL)



