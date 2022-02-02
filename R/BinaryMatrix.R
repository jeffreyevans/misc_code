# Create a binary matrix
#    n = size of matrix
#    lower.p = probability in lower triangle 
#    upper.p = probability in upper triangle 
binary.matrix <- function(n, lower.p=0.1, upper.p=0.9){
  vals <- sample(0:1, n*(n-1)/2, rep=TRUE, prob=c(lower.p,upper.p))
    mat <- matrix(0, n, n)
      mat[upper.tri(mat)] <- vals
    mat[lower.tri(mat)] <- vals
  return( mat )
}