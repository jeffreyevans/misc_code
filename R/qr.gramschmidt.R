#' Gram-Schmidt QR Decomposition
#'
#' @examples
#' A <- data.frame(v1=seq(0.1, 5, length=100), v2=seq(0.1, 5, length=100), 
#'                 v3=dnorm(runif(100)), v4=dnorm(runif(100))) 
#'  qr.gramschmidt(A)
#'
qr.gramschmidt <- function(x) {
  x <- as.matrix(x)
  n <- ncol(x)
  m <- nrow(x)
  q <- matrix(0, m, n)
  r <- matrix(0, n, n)  
    for (j in 1:n) {
      v = x[,j] # Step 1 of the Gram-Schmidt process v1 = a1
      if (j > 1) {
        for (i in 1:(j-1)) {
          r[i,j] <- t(q[,i]) %*% x[,j] # Find the inner product (noted to be q^T a earlier)
          # Subtract the projection from v which causes v to become perpendicular to all columns of Q
          v <- v - r[i,j] * q[,i] 
        }      
      }
      # Find the L2 norm of the jth diagonal of R
      r[j,j] <- sqrt(sum(v^2))
      # The orthogonalized result is found and stored in the ith column of Q.
      q[,j] <- v / r[j,j]
    } 
  # Return Q (qr) and R (rank) matrices  
  return( list('qr'=q, 'rank'=r) )
}

