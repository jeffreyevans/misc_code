#' Conditional AIC is a quantity is derived in a way analogous to the AIC, 
#' but is appropriate for scenarios where one is interested in the particular 
#' coefficient estimates for individual random effects. The formula for the 
#' asymptotic cAIC is given as:
#' -2*log(likelihood of observed values, conditional on ML estimates of fixed 
#' effects and empirical Bayes estimates of random effects) + 2*K
#'   where: K = rho + 1, and rho = "effective degrees of freedom" = trace of the 
#'          hat matrix mapping predicted values onto observed values.
#'
#' @references
#' Vaida, F. and S. Blanchard (2005) Conditional Akaike information 
#'   for mixed-effects models. Biometrika 92:351–370
#'
#' Sonja Greven and Thomas Kneib (2009) On the Behaviour of Marginal and Conditional Akaike 
#'   Information Criteria in Linear Mixed Models. Johns Hopkins University, 
#'   Dept. of Biostatistics Working Papers. Working Paper 202.
#'   http://www.bepress.com/jhubiostat/paper202 
#'
cAIC <- function(model) {
	 sigma <- attr(VarCorr(model), 'sc')
	  observed <- attr(model, 'y')
	   predicted <- fitted(model)
	   p <- length(fixef(model))
	  N <- nrow(attr(model, 'X'))
	 rho <- hatTrace(model)	
	cond.loglik <- sum(dnorm(observed, predicted, sigma, log=TRUE))
	 # K.corr is the finite-sample correction for K, for ML model fits.
	 K.corr <- N*(N-p-1)*(rho+1)/((N-p)*(N-p-2)) + N*(p+1)/((N-p)*(N-p-2))
	CAIC <- -2*cond.loglik + 2*K.corr
 return(CAIC)
}
