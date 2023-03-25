#' @title Covariance test of full and subsampled data
#'
#' @description Test for the equality of two sample covariance matrices.
#'
#' @param X        A \code{matrix} or \code{data.frame} object representing full data.
#' @param Y        A \code{matrix} or \code{data.frame} object representing sample data.
#' @param method   String indicating the method, current available
#'                 methods are \code{CLX} or \code{Scott}.
#' @param alpha    Significant level of the test.
#'
#' @return
#' List object with test statistic, p-value alternative and, method
#'
#' @note
#' The CLX method implements Cai et al., (2013) and the Scott method from Scott (2007) 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @references
#' Cai, T. T., Liu, W., and Xia, Y. (2013) Two-sample covariance matrix testing and 
#' support recovery in high-dimensional and sparse settings. Journal of the American 
#' Statistical Association 108, 265-277.
#'
#' Scott, J. R. (2007). A test for the equality of covariance matrices when the dimension 
#' is large relative to the sample size. Computational Statistics and Data Analysis 
#' 51, 6535-6542.
#'
#' @export
cov.test <- function(X, Y, method = c("Scott", "CLX"), alpha = 0.05) {
  method = tolower(method[1])  
  p = ncol(X)
    if (ncol(Y) != p)
      stop('Different dimensions of X and Y.')
    n1 = nrow(X)
      n2 = nrow(Y)
        tmp <- rep(c(rep(1, n1)/n1, rep(1, n2)/n2), 1)
      scalev <- matrix(tmp, ncol = 1)
    qalpha <- -log(8*pi) - 2*log(log(1/(1-alpha)))
  cri <- 4*log (p)-log (log (p)) + qalpha
  
  Sx <- cov(X)*(n1-1)/n1
    Sy <- cov(Y)*(n2-1)/n2
      xa <- t(t(X) - colMeans(X))
      ya <- t(t(Y) - colMeans(Y))
    vx <- t(xa^2)%*%(xa^2)/n1 - 2/n1 * (t(xa)%*% xa) * Sx + Sx^2
  vy <- t(ya^2)%*%(ya^2)/n2 - 2/n2 * (t(ya)%*% ya) * Sy + Sy^2

  numo <- abs(Sx-Sy)
    deno <- sqrt(vx/n1 + vy/n2)
      Tnm <- max(numo/deno)
  xat <- t(xa)/n1
  yat <- t(ya)/n2  
  scalev <- c(rep(1, n1)/n1, rep(1, n2)/n2)
    ts <- matrix(0,1,1)
      g <- rnorm(n1+n2)*scalev
        atmp <- sum(g[1:n1])
        btmp <- sum(g[(n1+1):(n1+n2)])
      ts1 <- (t(xa*g[1:n1]) - xat*atmp)%*% xa
    ts2 <- (t(ya*g[(n1+1):(n1+n2)]) - yat*btmp)%*% ya
  ts[1] <- max(abs(ts1-ts2)/deno)
  ZCZt <- Tnm > quantile(ts, 1-alpha)
    CLX <- max((Sx-Sy)^2/(vx/n1+vy/n2))
      Sxx <- Sx*n1/(n1-1)
        Syy <- Sy*n2/(n2-1)
      SsS <- (Sxx*n1 + Syy*n2)/(n1+n2)
    eta1 <- ((n1-1)+2)*((n1-1)-1)
  eta2 <- ((n2-1)+2)*((n2-1)-1)
  
  d1 <- (1-(n1-1-2)/eta1)*sum(diag(Sxx%*%Sxx))
    d2 <- (1-(n2-1-2)/eta2)*sum(diag(Syy%*%Syy))
      d3 <- 2*sum(diag(Sxx %*% Syy))
    d4 <- (n1-1)/eta1*sum(diag(Sxx))^2
  d5 <- (n2-1)/eta2*sum(diag(Syy))^2
  th <- 4*(((n1+n2-2)/((n1-1)*(n2-1)))^2)*
    ((n1+n2-2)^2/((n1+n2)*(n1+n2-2-1))*
    (sum(diag(SsS %*% SsS))-(sum(diag(SsS)))^2/(n1+n2-2)))^2
  Sc <- (d1+d2-d3-d4-d5)/sqrt(th)

  if(method[1] == "clx") {
    #### Two-Sample CLX test
    names(CLX) = "Statistic"
    CLX.rev <- CLX-(4*log(p)-log(log(p)))
      clx.p <- 1 - exp(-exp(-CLX.rev/2)/(sqrt(8*pi)))
    res <- list(statistics = CLX, p.value = clx.p, alternative = "two.sided",
               method = "Two-Sample CLX test")
  } else if(method[1] == "scott") {
    #### Two-Sample Scott test 
    names(Sc) = "Statistic"
    sc.p <- (1-pnorm(abs(Sc)))*2
    res <- list(statistics = abs(Sc), p.value = sc.p, alternative = "two.sided",
               method = "Two-Sample Scott test")
  }
  return(res)
}
