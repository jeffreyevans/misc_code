# Test bootstrap independence of a variable
#   n     Bootstrap size
#   p     Sample size 
#   idx   Column index
bootCorTest <- function(x, n=1000, p=0.68, idx=1, plot=TRUE) { 
  if (!inherits(x, "data.frame")) 
       stop(deparse(substitute(x)), " Must be a data.frame object") 
  BootCor=vector()  
    for(i in 1:n) {
	  BootCor <- append(BootCor, cor(x[sample(1:nrow(x), nrow(x)*p, replace=TRUE),][idx],
	                    x[sample(1:nrow(x), nrow(x)*p, replace=TRUE),][idx])[1])
    }
	if( plot == TRUE ) {
	  xDen <- density(BootCor)
        plot(xDen,type="n", main=paste("Bootstrap sample independence",
             names(x)[idx],sep=" - "), xlab="Correlation",ylab="PDF")
               polygon(xDen,col="blue")
	    }
  return( BootCor )
}

# Example on col 1 "Sepal.Length" of iris
data(iris)
boot.test <- bootCorTest(iris) 

# Matrix correlation average. If average=FALSE returns sum cor matrix
bootCorMat <- function(x, n=1000, p=0.68, average=TRUE) { 
  if (!inherits(x, "data.frame")) 
       stop(deparse(substitute(x)), " Must be a data.frame object")
    corMat <- cor(x[sample(1:nrow(x), nrow(x)*p, replace=TRUE),],
                  x[sample(1:nrow(x), nrow(x)*p, replace=TRUE),])	   
    for(i in 1:n-1) {
	  corMat <- corMat + cor(x[sample(1:nrow(x), nrow(x)*p, replace=TRUE),],
                  x[sample(1:nrow(x), nrow(x)*p, replace=TRUE),])	  
	} 
	if (average == TRUE) {corMat = corMat / n}
  return( corMat )	
}	   

data(iris)
x=iris[,1:4]
bootCorMat(x)

