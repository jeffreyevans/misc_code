##########################################################################
# PROGRAM: bootImp 
# USE: BAYESIAN BOOTSTRAP IMPUTATION  
# REQUIRES: R >= 2.12.0 
#           Hmisc 
#
# ARGUMENTS: 
#       formula           OBJECT OF CLASS formula, SYMBOLIC DESCRIPTION OF MODEL     
#       data              DATAFRAME        
#       n.impute          NUMBER OF VALUES TO IMPUTE (USED TO AVERAGE PREDICTION)              
#       nk                N USED FOR IMPUTATION (DEFAULT=3)
#       type              "pmm" OR "regression"
#       boot.method       "simple" OR "approximate bayesian"            
#
#  VALUE:
#     DATAFRAME OBJECT WITH COLUMNS FOR EACH K AND PREDICTED VALUE
#                  
#  EXAMPLES: 
# setwd("C:/ANALYSIS/TEST")
# data <- read.csv("ALL_DATA.csv", header=TRUE)
#  md.imp <- bootImp(~YVar + v1 + v2 + v3, data=data, n.impute=3, nk=3,
#              type="regression",  match="weighted", curtail=TRUE, 
#              boot.method=c("simple"), B=1000) 
#  print(md.imp)
#  impYVar <- as.data.frame(md.imp$imputed$YVar)   
# THE BOOTSTRAP ESTIMATE OF THE MISSING VALUE IS THE MEAN OF IMPUTED n
# HERE ARE THE 4 MISSING VALUES I TESTED WITH 3 BOOTSTRAP ESTIMATES
#                 V1       V2       V3     PRED
#        1  3.705537 72.76648 96.00000  57.49067233
#        2  0.000000 61.82800 47.19317  36.34039   
#        3 29.778937 52.82379 30.45202  37.68491567
#        4 27.780687 56.61398 96.00000  60.13155567
#
# CONTACT: 
#     Jeffrey S. Evans 
#     Senior Landscape Ecologist 
#     The Nature Conservancy,
#     Development by Design
#     Affiliate Assistant Professor
#     University of Wyoming,
#     Zoology & Physiology 
#     Laramie, WY
#     (970)672-6766
#     jeffrey_evans@tnc.org
############################################################# 
bootImp <- function (formula, data, subset, n.impute=5, group=NULL, 
                      nk=3, tlinear=TRUE, type=c("pmm", "regression"), 
					  match=c("weighted", "closest"), fweighted=0.2, 
					  curtail=TRUE, boot.method=c("simple", "approximate bayesian"), 
					  burnin=3, x=FALSE, pr=TRUE, plotTrans=FALSE, tolerance=NULL, B=75) 
    {
    if (!require(Hmisc)) stop("Hmisc PACKAGE MISSING")
    acall <- match.call()
    type <- match.arg(type)
    match <- match.arg(match)
    boot.method <- match.arg(boot.method)
    if (!inherits(formula, "formula")) 
        stop("formula must be a formula")
    nam <- var.inner(formula)
    m <- match.call(expand = FALSE)
    Terms <- terms(formula, specials = "I")
    m$formula <- formula
    m$match <- m$fweighted <- m$curtail <- m$x <- m$n.impute <- m$nk <- m$tlinear <- m$burnin <- m$type <- m$group <- m$pr <- m$plotTrans <- m$tolerance <- m$boot.method <- m$B <- NULL
    m$na.action <- na.retain
    m[[1]] <- as.name("model.frame")
    z <- eval(m, sys.parent())
    p <- length(z)
    n <- nrow(z)
    rnam <- row.names(z)
    if (length(rnam) == 0) 
        rnam <- as.character(1:n)
    lgroup <- length(group)
    if (lgroup) {
        if (boot.method == "approximate bayesian") 
            stop("group not implemented for boot.method=\"approximate bayesian\"")
        if (lgroup != n) 
            stop("group should have length equal to number of observations")
        ngroup <- length(unique(group[!is.na(group)]))
    }
    linear <- nam[attr(Terms, "specials")$I]
    cat.levels <- vector("list", p)
    names(cat.levels) <- nam
    vtype <- rep("s", p)
    names(vtype) <- nam
    dof <- rep(NA, p)
    names(dof) <- nam
    na <- vector("list", p)
    names(na) <- nam
    nna <- integer(p)
    names(nna) <- nam
    xf <- matrix(as.double(1), nrow = n, ncol = p, dimnames = list(rnam, 
        nam))
    imp <- vector("list", p)
    names(imp) <- nam
    if (lgroup) 
        group.inds <- imp
    for (i in 1:p) {
        xi <- z[[i]]
        ni <- nam[i]
        nai <- is.na(xi)
        na[[i]] <- (1:n)[nai]
        nna[i] <- nnai <- sum(nai)
        if (nnai > 0) 
            imp[[ni]] <- matrix(NA, nrow = nnai, ncol = n.impute, 
                dimnames = list(rnam[nai], NULL))
        if (lgroup) {
            if (any(is.na(group[!nai]))) 
                stop("NAs not allowed in group")
            if (length(unique(group[!nai])) != ngroup) 
                stop(paste("not all", ngroup, "values of group are represented in\n", 
                  "observations with non-missing values of", 
                  ni))
            group.inds[[i]] <- split((1:n)[!nai], group[!nai])
        }
        iscat <- FALSE
        if (is.character(xi)) {
            xi <- as.factor(xi)
            lev <- levels(xi)
            iscat <- TRUE
        }
        else if (is.category(xi)) {
            lev <- levels(xi)
            iscat <- TRUE
        }
        if (iscat) {
            cat.levels[[ni]] <- lev
            xi <- as.integer(xi)
            vtype[ni] <- "c"
        }
        else {
            u <- unique(xi[!nai])
            if (length(u) == 1) 
                stop(paste(ni, "is constant"))
            else if ((length(nk) == 1 && nk == 0) || length(u) == 
                2 || ni %in% linear) 
                vtype[ni] <- "l"
        }
        xf[, i] <- xi
        if (nnai > 0) 
            xf[nai, i] <- sample(xi[!nai], nnai, replace = nnai > 
                (n - nnai))
    }
    z <- NULL
    wna <- (1:p)[nna > 0]
    rsq <- double(length(wna))
    names(rsq) <- nam[wna]
    resampacc <- list()
    if (curtail) 
        xrange <- apply(xf, 2, range)
    for (iter in 1:(burnin + n.impute)) {
        if (pr) 
            cat("Iteration", iter, "\r")
        for (i in wna) {
            nai <- na[[i]]
            j <- (1:n)[-nai]
            npr <- length(j)
            ytype <- if (tlinear && vtype[i] == "s") 
                "l"
            else vtype[i]
            if (iter == (burnin + n.impute) && length(nk) > 1) {
                rn <- c("Bootstrap bias-corrected R^2", "10-fold cross-validated  R^2", 
                  "Bootstrap bias-corrected mean   |error|", 
                  "10-fold cross-validated  mean   |error|", 
                  "Bootstrap bias-corrected median |error|", 
                  "10-fold cross-validated  median |error|")
                racc <- matrix(NA, nrow = 6, ncol = length(nk), 
                  dimnames = list(rn, paste("nk=", nk, sep = "")))
                jj <- 0
                for (k in nk) {
                  jj <- jj + 1
                  f <- areg(xf[, -i, drop = FALSE], xf[, i], 
                    xtype = vtype[-i], ytype = ytype, nk = k, 
                    na.rm = FALSE, tolerance = tolerance, B = B, 
                    crossval = 10)
                  w <- c(f$r2boot, f$rsquaredcv, f$madboot, f$madcv, 
                    f$medboot, f$medcv)
                  racc[, jj] <- w
                }
                resampacc[[nam[i]]] <- racc
            }
            if (lgroup) {
                s <- rep(NA, npr)
                for (ji in 1:ngroup) {
                  gi <- (group.inds[[i]])[[ji]]
                  s[gi] <- sample(gi, length(gi), replace = TRUE)
                }
            }
            else {
                s <- sample(j, npr, replace = TRUE)
                if (boot.method == "approximate bayesian") 
                  s <- sample(s, replace = TRUE)
            }
            nami <- nam[i]
            nm <- c(nami, nam[-i])
            X <- xf[, -i, drop = FALSE]
            f <- areg(X[s, ], xf[s, i], xtype = vtype[-i], ytype = ytype, 
                nk = min(nk), na.rm = FALSE, tolerance = tolerance)
            dof[names(f$xdf)] <- f$xdf
            dof[nami] <- f$ydf
            if (plotTrans) 
                plot(f)
            rsq[nami] <- f$rsquared
            pti <- predict(f, X)
            if (type == "pmm") {
                if (ytype == "l") 
                  pti <- (pti - mean(pti))/sqrt(var(pti))
                whichclose <- if (match == "closest") {
                  pti[j] <- pti[j] + runif(npr, -1e-04, 1e-04)
                  j[whichClosest(pti[j], pti[nai])]
                }
                else j[whichClosePW(pti[j], pti[nai], f = fweighted)]
                impi <- xf[whichclose, i]
            }
            else {
                res <- f$residuals
                r <- sample(res, length(nai), replace = length(nai) > 
                  length(res))
                ptir <- pti[nai] + r
                impi <- f$yinv(ptir, what = "sample", coef = f$ycoefficients)
                if (curtail) 
                  impi <- pmin(pmax(impi, xrange[1, i]), xrange[2, 
                    i])
            }
            xf[nai, i] <- impi
            if (iter > burnin) 
                imp[[nam[i]]][, iter - burnin] <- impi
        }
    }
    if (pr) 
        cat("\n")
    if (!x) 
        xf <- NULL
    structure(list(call = acall, formula = formula, match = match, 
        fweighted = fweighted, n = n, p = p, na = na, nna = nna, 
        type = vtype, tlinear = tlinear, nk = min(nk), cat.levels = cat.levels, 
        df = dof, n.impute = n.impute, imputed = imp, x = xf, 
        rsq = rsq, resampacc = resampacc), class = "aregImpute")
}
        