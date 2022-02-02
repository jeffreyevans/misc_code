# COMPAIR TWO MATRICES
m1 <- as.matrix(data.frame(cbind(x1=runif(1000, min=1, max=10),
             x2=runif(1000, min=0, max=1),
             x4=runif(1000, min=0.5, max=1.5))))
m2 <- as.matrix(data.frame(cbind(x1=runif(100, min=1, max=10),
             x2=runif(1000, min=0, max=1),
             x4=runif(1000, min=0.25, max=1.75))))
MatrixEquality(as.matrix(m1), as.matrix(m2))

# COMPAIR COVARIANCES OF MATRICES (USING SUBSAMPLE)
s <- sample(1:nrow(m1), 100) 
  cov.all <- cov(m1)
    cov.sub <- cov(m1[s,])
MatrixEquality(cov.all, cov.sub)

# COMPAIR EQUALITY OF TWO MATRICES
MatrixEquality <- function(m1, m2) {
   k = 2
    p = 2
     n1 = dim(m1)[1]
      n2 = dim(m2)[1] 
       n = n1 + n2
        s1 <- crossprod(m1[1:dim(m1)[1]])
         s2 <- crossprod(m2[1:dim(m2)[1]])
          c1 = (1/(n1-1))*s1
         c2 = (1/(n2-1))*s2
         c3 = (s1+s2)/(n-k)
        d = det(c3)
        d1 = det(c1)
       d2 = det(c2) 
      m = ( (n-k)*log(d) ) - ( (n1-1)*log(d1) + (n2-1)*log(d2) )
     h = 1-((2*p*p+3*p-1) / (6*(p+1)*(k-1)) * (1 / (n1-1)+1 / (n2-1)+1 / (n-k)))
    chi = abs(m*h)
   dfree = p*(p+1)*(k-1)/2
   print( paste("DEGREES OF FREEDOM", dfree, sep=":") )
     print( paste("p", chi, sep="=") )
        if ( (chi < 1 ) == TRUE ) {
          list (TEST="ACCEPT NULL", DF=dfree, p=chi)
            } else {
          list (TEST="REJECT NULL", DF=dfree, p=chi) 
    }
}

