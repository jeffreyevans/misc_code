# Geographically Weighted PCA

# Lets begin with the weights, because this is where geographically weighted 
# PCA parts company from PCA itself. The term "geographical" means the weights 
# depend on distances between a base point and the data locations. The standard,
# but by no means only, weighting is a Gaussian function; that is, exponential 
# decay with squared distance. The user needs to specify the decay rate or, more 
# intuitively, a characteristic distance over which a fixed amount of decay occurs.

# x is a vector location
# xy is an array of locations, one per row
# tau is the bandwidth
# Returns a vector of weights
distance.weight <- function(x, xy, tau=1) {
    apply(xy, 1, function(z) exp(-(z-x) %*% (z-x) / (2 * tau^2)))
  }

# PCA applies either to a covariance or correlation matrix (which is derived from a covariance). 
# Here, then, is a function to compute weighted covariances in a numerically stable way.

# y is an m by n matrix
# weights is length m
# Returns the weighted covariance matrix of y (by columns).
covariance <- function(y, weights) {
  if (missing(weights)) return (cov(y))
    w <- zapsmall(weights / sum(weights))  # Standardize the weights
     y.bar <- apply(y * w, 2, sum)         # Compute column means
     z <- t(y) - y.bar                     # Remove the means
    z %*% (w * t(z))  
  }

# The correlation is derived in the usual way, by using the standard deviations for the 
# units of measurement of each variable:
correlation <- function(y, weights) {
    z <- covariance(y, weights)
      sigma <- sqrt(diag(z))  # Standard deviations
    z / (sigma %o% sigma)
  }

# Weighted PCA function:
# x is a vector denoting a location
# xy is a set of locations as row vectors
# y is an array of attributes, also as rows
# tau is a bandwidth
# Returns a `princomp` object for the geographically weighted PCA
#   of y relative to the point x.
gw.pca <- function(x, xy, y, tau) {
    w <- distance.weight(x, xy, tau)
    princomp(covmat=correlation(y, w))
  }

################################################################################
# EXAMPLE 
################################################################################
# Let's illustrate with some random sample data comparable to those described in 
# the question: 30 variables at 550 locations.

set.seed(17)
n.data <- 550
n.vars <- 30
xy <- matrix(rnorm(n.data * 2), ncol=2)
y <- matrix(rnorm(n.data * n.vars), ncol=n.vars)

# Geographically weighted calculations are often performed on a selected set of 
# locations, such as along a transect or at points of a regular grid. Let's use 
# a coarse grid to get some perspective on the results; later, once we're confident 
# everything is working and we are getting what we want, we can refine the grid.

# Create a grid for the GWPCA, sweeping in rows from top to bottom.
xmin <- min(xy[,1]); xmax <- max(xy[,1]); n.cols <- 30
  ymin <- min(xy[,2]); ymax <- max(xy[,2]); n.rows <- 20
    dx <- seq(from=xmin, to=xmax, length.out=n.cols)
      dy <- seq(from=ymin, to=ymax, length.out=n.rows)
  
points <- cbind(rep(dx, length(dy)), as.vector(sapply(rev(dy), 
                function(u) rep(u, length(dx)))))

# There's a question of what information we wish to retain from each PCA. Typically, 
# a PCA for n variables returns a sorted list of n eigenvalues and, in various forms,
# a corresponding list of n vectors, each of length n. That's n*(n+1) numbers to map! 
# Taking some cues from the question, let's map the eigenvalues. These are extracted 
# from the output of gw.pca via the $sdev attribute, which is the list of eigenvalues 
# by descending value.

# Illustrate GWPCA by obtaining all eigenvalues at each grid point.
z <- apply(points, 1, function(x) gw.pca(x, xy, y, 1)$sdev)

# CREATE RASTERS OF EIGENVALUES AND PLOT RESULTS
library(raster)
to.raster <- function(u) raster(matrix(u, nrow=n.cols), xmn=xmin, xmx=xmax, 
                                ymn=ymin, ymx=ymax)
maps <- apply(z, 1, to.raster)
  par(mfrow=c(2,2))
    plot(maps[[1]]); points(xy, pch=20, cex=0.60)
    plot(maps[[2]]); points(xy, pch=20, cex=0.60)
    plot(maps[[3]]); points(xy, pch=20, cex=0.60)
    plot(maps[[4]]); points(xy, pch=20, cex=0.60)

# Plot all  
# tmp <- lapply(maps, function(m) {plot(m); points(xy, pch=19)})
				