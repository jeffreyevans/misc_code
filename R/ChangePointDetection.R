#################################
# changepoint.np library approach
library(changepoint.np)
set.seed(12)
J <- function(x){(1+sign(x))/2}
n <- 1000
tau <- c(0.1,0.13,0.15,0.23,0.25,0.4,0.44,0.65,0.76,0.78,0.81)*n
h <- c(2.01, -2.51, 1.51, -2.01, 2.51, -2.11, 1.05, 2.16,-1.56, 2.56, -2.11)
sigma <- 0.5
t <- seq(0,1,length.out = n)
data <- array()
for (i in 1:n){
   data[i] <- sum(h*J(n*t[i] - tau)) + (sigma * rnorm(1))
}

ts.plot(data)
out <- changepoint.np::cpt.np(data, method = "PELT", minseglen = 2, 
                              nquantiles = 4*log(length(data)))
( cp <- cpts(out) ) #change point values
  plot(out)
  
#################################
# ecp library approach
#   This can be applied to 
#   multivariate data
library("ecp")
set.seed(250)

# Univariate example
period1 <- rnorm(100)
period2 <- rnorm(100, 0, 3)
period3 <- rnorm(100, 2, 1)
period4 <- rnorm(100, 2, 4)
Xnorm <- matrix(c(period1, period2, period3, period4), ncol = 1)
output1 <- e.divisive(Xnorm, R = 499, alpha = 1)
output2 <- e.divisive(Xnorm, R = 499, alpha = 2)
output2$estimates 

ts.plot(Xnorm, ylab = "Value", main = "Change in a Univariate Gaussian Sequence")
  abline(v = c(101, 201, 301), col = "blue")
  abline(v = output1$estimates[c(-1, -5)], col = "red", lty = 2)
  
  
# Multivariate example  
library("mvtnorm")
set.seed(200)
mu <- rep(0, 3)
covA <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), 3, 3)
covB <- matrix(c(1, 0.9, 0.9, 0.9, 1, 0.9, 0.9, 0.9, 1), 3, 3)
period1 <- rmvnorm(250, mu, covA)
period2 <- rmvnorm(250, mu, covB)
period3 <- rmvnorm(250, mu, covA)
Xcov <- rbind(period1, period2, period3)

DivOutput <- ecp::e.divisive(Xcov, R = 999, alpha = 1)
  DivOutput$estimates





 