

# LINNEAR FIT
data(cars)
m <- lm( cars$dist ~ cars$speed )
k <- coef(m)
plot( cars$dist ~ cars$speed, pch=19 )
curve( k[1] + k[2]*x , from=min(cars$speed) ,to=max(cars$speed) , add=TRUE )
plot( cars$dist ~ cars$speed, pch=19, ylab="y",xlab="x" )
curve( k[1] + k[2]*x , from=min(cars$speed) ,to=max(cars$speed) , add=TRUE )

# NORMAL DISTRIBUTION
x=seq(-4,4,length=200)
y=1/sqrt(2*pi)*exp(-x^2/2)
plot(x,y,type="l",lwd=2,col="red")
plot(x,y,type="l",lwd=2,col="black")
plot(x,y,type="l",lwd=2,col="black")

# MOMENTS (NORMAL DISTRIBUTION)
x=seq(-4,4,length=200)
y=dnorm(x)
plot(x,y,type="l", lwd=2, col="black", ylab="y",xlab="x")
  abline(v=mean(x))
   abline(v=sd(x), lty=3)
    abline(v=-sd(x), lty=3)
   abline(v=quantile(x,p=0.10), col="red", lty=4)
  abline(v=quantile(x,p=0.95), col="red", lty=4)

# SKEWED DISTRIBUTION
x=rweibull(1e5,1.5,33)
plot(density(x), ylab="y",xlab="x", main="")
  abline(v=mean(x))
   abline(v=sd(x), lty=3)
    abline(v=-sd(x), lty=3)
   abline(v=quantile(x,p=0.10), col="red", lty=4)
  abline(v=quantile(x,p=0.95), col="red", lty=4)


# MOMENTS (BIMODAL DISTRIBUTION)
library(e1071) 
plot(density(faithful$eruptions), ylab="y",xlab="x", main="")
  abline(v=mean(faithful$eruptions))
   abline(v=sd(faithful$eruptions), lty=3)
    abline(v=-sd(faithful$eruptions), lty=3)
 
# PROB OF DRAWIND +- 1 SD
x=seq(-4,4,length=200)
y=dnorm(x)
plot(x,y,type="l", lwd=2, col="black")
  x=seq(-1,1,length=100)
  y=dnorm(x)
    polygon(c(-1,x,1),c(0,y,0),col="blue")

	