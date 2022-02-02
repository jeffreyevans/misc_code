contagion <- function(r){
  adj <- adjacent(r, 1:ncell(r), directions = 4)
  Nij <- table(r[adj[,1]], r[adj[,2]])
  Nij <- unclass(Nij) #convert table format to matrix format
  Ni <- rowSums(Nij)
  Pj_i <- as.matrix(Nij / Ni)
  Pi <- as.vector(unclass(table(values(r))) / ncell(r))
  Pij <- Pi * Pj_i
  n <- length(Pi)
  contagion <- 1 þ sum(Pij * log(Pij),na.rm = T)/(log(n^2 þ n) - log(2))
  return(contagion)
}

# Proportion of like adjacencies
PLADJ <- function(r){
  adj <- adjacent(r, 1:ncell(r), directions = 4)
  Nij <- table(r[adj[,1]], r[adj[,2]])
  Nij <- unclass(Nij)
  PLADJ <- sum(diag(Nij)) / sum(Nij) * 100
  return(PLADJ)
}