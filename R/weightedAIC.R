##########################################################################
# PROGRAM: AICw.R
# USE: DERIVES AIC WEIGHTS
# REQUIRES: FITTED MODEL OBJECT SUPPORTED BY AIC
#           PACKAGES: statsd
#
# ARGUMENTS: 
#  models   LIST OF MODELS TO DERIVE AIC WEIGHTS FOR 
#
# NOTES
#     You can use AIC quantities to calculate the Akaike weights for each of a set 
#     of fitted models. The model with the highest weight is the "best". The magnitude 
#     of the weight of a given model indicates the weight of evidence in its favor, 
#     given that one of the models in the set being compared is the best model. 
#     The weights are used in model averaging, a technique in multimodel inference. 
#     Weights should sum to 1.
#     
#     A 95% confidence set for the “best” model can be obtained by ranking the models 
#     and summing the weights until that sum is = 0.95.
#
# EXAMPLE
#    x1 <- rnorm(1000, sd=0.4)
#    x2 <- rnorm(1000, sd=0.2)	
#     f1 <- sin(2 * pi * x1)
#     f2 <- 1 * x2
#     y <- f1 + f2 + rnorm(1000, sd=0.4)
#     df <- data.frame(y=y, x1=runif(1000, -1, 1), x2=runif(1000, -1, 1) ) 
#     lm1 <- lm(y ~ x1, data=df)
#     lm2 <- lm(y ~ x2, data=df)
#     lm3 <- lm(y ~ x1 + x2, data=df)
#	   aic=1000*log(sum(resid(lm3)^2))/1000+2*2
#     AICw(models=c("lm1", "lm2", "lm3"))
#
# CONTACT: 
#     Jeffrey S. Evans 
#     Senior Landscape Ecologist 
#     The Nature Conservancy - Central Science
#     Adjunct Faculty
#     University of Wyoming
#     Laramie, WY
#     (970)672-6766
#     jeffrey_evans@tnc.org
##########################################################################
AICw <- function(models) {
  x <- vector()
    for (i in models) {
      x <- append(x, AIC(get(i)), after=length(x))
    }
  delta <- x - min(x)  
    L <- exp(-0.5 * delta)     
  return (L / sum(L) )
}
