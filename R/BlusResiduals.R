# Blus residuals, removes autocorrelation in regression residuals 
# Theil (1968) proposed a transformation of regression residuals so 
# that they are best, unbiased, linear, scalar (BLUS). 
#    
#' Vinod, H.D., T. Blus (2014) Residuals and R Tools for Testing and Removing 
#'   Autocorrelation and Heteroscedasticity. 
#    
#' @examples 
#     require(sp)
#       data(meuse)
#     ( bresd <- blus.residuals(meuse$copper, meuse$lead) ) 
#     ( bresd <- blus.residuals(meuse$copper, cbind(meuse$lead,meuse$zinc)) ) 
#
blus.residuals <- function(y, x) {
  T <- length(y)
    u <- resid(lm(y ~ x))
      ane <- rep(1, T)
        bigx <- cbind(ane, x)
            p <- NCOL(bigx)
          u0 <- u[1:p]
        u1 <- u[(p + 1):T]  # THIS SEEMS TO BE CAUSING THE LOSS OF n OBS
      X0 <- bigx[1:p, 1:p]  # THIS SEEMS TO BE CAUSING THE LOSS OF n OBS
    X0inv <- solve(X0)
  X1 <- bigx[(p + 1):T, ]   # THIS SEEMS TO BE CAUSING THE LOSS OF n OBS
  xtxinv <- solve(t(bigx) %*% bigx)
    mtx <- X0 %*% xtxinv %*% t(X0)
      ei <- eigen(mtx, symmetric=TRUE)
          disq <- ei$values
        di <- sqrt(disq)
      c1 <- di/(1 + di) 
    q <- ei$vectors 
  sum1 <- matrix(0, p, p) 
    for (i in 1:p) {
      qi <- q[, i] 
        mtx2 <- qi %*% t(qi) 
      sum1 <- sum1 + (c1[i] * mtx2) 
    } 
    m1 <- X0inv %*% sum1 
      v1 <- m1 %*% u0 
        u1blus <- u1 - X1 %*% v1 
    r <- data.frame(raw=u,corrected=as.vector(u1blus))
      rownames(r) <- names(u)	
        return(list(residuals=r, eigenvectors=q) )
  }

  
bresd$lm.residuals, 
bresd$residuals  

data.frame



  