X <- iris[,1:4]
y <- iris[,5]
y_col <- c('#7DB0DD', '#86B875', '#E495A5')
pairs(X, lower.panel = NULL, col = y_col[y], pch=19, cex=1.5)
  par(xpd = T)
    legend(x = 0.1, y = 0.4, legend = as.character(levels(y)), 
	       pch=c(19,19,19), col = y_col)


gmm.model <- gmm(X, 3)
  pairs(X, lower.panel = NULL, col = gmm.model$cluster)
    table(y, gmm.model$cluster)


# finds partition such that squared error between empirical mean
# and points in cluster is minimized over all k clusters
km <- function(X, k){
  p <- ncol(X)  # number of parameters
  n <- nrow(X)  # number of observations
  Delta <- 1; iter <- 0; itermax <- 30
  while(Delta > 1e-4 && iter <= itermax){
    # initiation
    if(iter == 0){
      centroid <- X[sample(nrow(X), k),]
      centroid_mem <- centroid
    }
    
    # equivalent to E-step
    d <- sapply(1:k, function(c) sapply(1:n, 
      function(i) sum((centroid[c,] - X[i,])^2) ))
    cluster <- apply(d, 1, which.min)
    
    # equivalent to M-step
    centroid <- t(sapply(1:k, function(c) 
      apply(X[cluster == c,], 2, mean)))
    
    Delta <- sum((centroid - centroid_mem)^2)
    iter <- iter + 1; centroid_mem <- centroid
  }
  return(list(centroid = centroid, cluster = cluster))
}


# Uses EM algorithm with multivariate normal
# distribution to estimate cluster probability
mvnorm.cov.inv <- function(Sigma) {
  # Eigendecomposition of covariance matrix
  E <- eigen(Sigma)
  Lambda.inv <- diag(E$values^-1)   # diagonal matrix
  Q <- E$vectors
  return(Q %*% Lambda.inv %*% t(Q))
}

#multivariate Gaussian pdf
mvn.pdf.i <- function(xi, mu, Sigma) {
  1/sqrt( (2*pi)^length(xi) * det(Sigma) ) * 
  exp(-(1/2) * t(xi - mu) %*% mvnorm.cov.inv(Sigma) 
  %*% (xi - mu)  )
} 
 
mvn.pdf <- function(X, mu, Sigma) {
  apply(X, 1, function(xi) mvn.pdf.i(as.numeric(xi), mu, Sigma))
}
  
gmm <- function(X, k){
  p <- ncol(X)  # number of parameters
  n <- nrow(X)  # number of observations
  Delta <- 1; iter <- 0; itermax <- 30
  while(Delta > 1e-4 && iter <= itermax){
    # initiation
    if(iter == 0){
      km.init <- km(X, k)
      mu <- km.init$centroid; mu_mem <- mu
      w <- sapply(1:k, function(i) length(which(km.init$cluster == i)))
      w <- w/sum(w)
      cov <- array(dim = c(p, p, k))
      for(i in 1:p) { 
	    for(j in 1:p) { 
	      for(c in 1:k) { cov[i, j, c] <- 1/n * sum((X[km.init$cluster == c, i] - 
		                              mu[c, i]) * (X[km.init$cluster == c, j] - 
									  mu[c, j]))
	      }
        }
      }	
    }
    # E-step
    mvn.c <- sapply(1:k, function(c) mvn.pdf(X, mu[c,], cov[,, c]))
    r_ic <- t(w*t(mvn.c)) / rowSums(t(w*t(mvn.c))) 
    # M-step
    n_c <- colSums(r_ic)
    w <- n_c/sum(n_c)
    mu <- t(sapply(1:k, function(c) 1/n_c[c] * colSums(r_ic[, c] *
      X)))
    for(i in 1:p) for(j in 1:p) for(c in 1:k) cov[i, j, c] <-
      1/n_c[c] * sum(r_ic[, c] * (X[, i] - mu[c, i]) * r_ic[, c] *
      (X[, j] - mu[c, j]))    
	Delta <- sum((mu - mu_mem)^2)
    iter <- iter + 1; mu_mem <- mu
  }
  return(list(softcluster = r_ic, cluster = apply(r_ic, 1, which.max)))
}
