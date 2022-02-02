# SIMULATING MULTIVARIATE DATA
# lets first simulate a bivariate normal sample
library(MASS)

# Simulate bivariate normal data
mu <- c(0,0)                         # Mean
Sigma <- matrix(c(1, .5, .5, 1), 2)  # Covariance matrix

# Generate sample from N(mu, Sigma)
bivn <- mvrnorm(5000, mu = mu, Sigma = Sigma )  # from Mass package

# Calculate kernel density estimate
bivn.kde <- MASS::kde2d(bivn[,1], bivn[,2], n = 50)  
  image(bivn.kde)      
  contour(bivn.kde, add = TRUE)  
  
# Classic Bivariate Normal Diagram
library(ellipse)
rho <- cor(bivn)
y_on_x <- lm(bivn[,2] ~ bivn[,1])    # Regressiion Y ~ X
x_on_y <- lm(bivn[,1] ~ bivn[,2])    # Regression X ~ Y
plot_legend <- c("99% CI green", "95% CI red","90% CI blue",
                 "Y on X black", "X on Y brown")
 
plot(bivn, xlab = "X", ylab = "Y",
     col = "dark blue",
     main = "Bivariate Normal with Confidence Intervals")
lines(ellipse(rho), col="red")       # ellipse() from ellipse package
lines(ellipse(rho, level = .99), col="green")
lines(ellipse(rho, level = .90), col="blue")
abline(y_on_x)
abline(x_on_y, col="brown")
legend(3,1,legend=plot_legend,cex = .5, bty = "n")

# Three dimensional surface
# Basic perspective plot
persp(bivn.kde, phi = 45, theta = 30, shade = .1, border = NA) # from base graphics package
 
# RGL interactive plot
library(rgl)
col2 <- heat.colors(length(bivn.kde$z))[rank(bivn.kde$z)]
  persp3d(x=bivn.kde, col = col2)

# Draw from multi-t distribution without truncation
library (tmvtnorm)
Sigma <- matrix(c(1, .1, .1, 1), 2)  # Covariance matrix
X1 <- rtmvt(n=1000, mean=rep(0, 2), sigma = Sigma, df=2) # from tmvtnorm package
 
t.kde <- kde2d(X1[,1], X1[,2], n = 50)   # from MASS package
col2 <- heat.colors(length(bivn.kde$z))[rank(bivn.kde$z)]
persp3d(x=t.kde, col = col2)

#Higher Dimensional Distributions
library(corrplot)
library(clusterGeneration)
mu <- rep(0,10)
pdMat <- genPositiveDefMat(10,lambdaLow=10)
Sigma <- pdMat$Sigma
dim(Sigma)
mvn <- mvrnorm(5000, mu = mu, Sigma = Sigma )
 
corrplot(cor(mvn), 
         method="ellipse",
         tl.pos="n",
         title="Matrix Correlations")