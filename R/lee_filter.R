#####################################################################################
#' @export lee_filter
lee_filter <- function(x, s = c(9,9)) {
  xhat = terra::focal(x, w=matrix(1, s[1], s[2]), mean) 
    sqr_xhat = terra::focal(x^2, w=matrix(1, s[1], s[2]), mean)
      sigma = sqr_xhat - xmean^2
        gvar = as.numeric(stats::var(terra::values(x),na.rm=TRUE))[1]
      wts = sigma / ( sigma + gvar )
    lee = xhat + wts * (x - xhat)
  return(lee)
} 

#library(terra)
#library(rjson)
#
#setwd("C:/evans/myanmar/data/SAR")
#
#j <- fromJSON(file = "S1A_IW_GRDH_1SDV_20200102T233109_20200102T233134_030628_03826A_34E5.json")
#granual <- j$id
#  bb <- data.frame(do.call(rbind, 
#    list(j$footprint$coordinates[[1]][[1]][[2]],
#    j$footprint$coordinates[[1]][[1]][[3]],
#    j$footprint$coordinates[[1]][[1]][[4]],
#    j$footprint$coordinates[[1]][[1]][[5]])))
#      names(bb) <- c("x", "y")
#e <- terra::ext(c(min(bb$x), max(bb$x), min(bb$y), max(bb$y)))
#
#vv <- rast("S1A_IW_GRDH_1SDV_20200102T233109_20200102T233134_030628_03826A_34E5_vv.tiff") 
#  ext(vv) <- e 
#x <- crop(vv, ext(95.85759, 96.07424, 23.90829, 24.11127))
#y <- matrix(as.numeric(spatSample(x, 25, "regular")[,1]),5,5)
#
#K_DEFAULT = 1.0
#CU_DEFAULT = 0.523
#CMAX_DEFAULT = 1.73
#
######################################################################################
## Supporting functions
#
## weighting
## Computes the weighting function for Lee filter using cu as the 
## noise coefficient.
## cu is the noise variation coefficient
#
## wt <- focal(x, w=matrix(1,9,9), weighting)
#weighting <- function(y, cu = 0.523) {
#  ci <- stats::sd(y, na.rm=TRUE) / mean(y, na.rm=TRUE)
#  bw <- 1.0 - (cu^2 / ci^2)
#    if(w > 0) w = w else w = 0
#  return(w)
#}
#
#
#Rvar = as.numeric(stats::var(terra::values(x), na.rm=TRUE))
#varK = stats::var(y, na.rm=TRUE)
#meanK = mean(y, na.rm=TRUE)
#W = varK /(varK + Rvar)
#
#
#global_variance = 4.909217
#xmean = mean(as.numeric(y))
#sqr_mean = mean(as.numeric(y)^2)
#sigma =  sqr_mean - xmean^2
#wts = sigma / ( sigma + global_variance )
#
## Original Lee Sigma Filter
#
#
#
#
## Code from:
## https://github.com/aalling93/Custom-made-SAR-speckle-reduction
#ENL = 4.5
#s = matrix(1, 7, 7)
#sigma_v = sqrt(1/ENL)
#
## Local values 
#E_norm = focal(x, w=s, mean) 
#Var_y_norm = focal(E_norm, w=s, var)  
#Var_x_norm = (Var_y_norm - E_norm^2 * sigma_v^2) / (1+sigma_v^2)
#
#kmask = Var_x_norm / Var_y_norm
#kmask[kmask < 0] <- 0
#
## Need to figure this threshold approach out
#CFAR_lee = CFAR(kmask)
#threshold = mean(CFAR_lee)
#
## Local values with mask, sets k value under threshold 
## to have no mask 
#kmask[kmask > threshold] <- NA
#E = focal(mask(x, kmask), s, mean)
#
#Var_y = focal(Im, E, n, kmask, var)
#
## Weight 
#Var_x = (Var_y - E.^2*sigma_v^2)./(1+sigma_v^2);
#k = Var_x./Var_y ;
#k(k<0)=0;
#
## ouput
#Im = E + k. * (Im-E)
#
#
#
#
#
#
#
#
#
#
######## MATLAB ##################
#sigma_v = sqrt(1/ENL);
#%% Local values with out mask
#E_norm = localMean(Im, n);
#Var_y_norm = localVariance(Im,E_norm, n);
#Var_x_norm = (Var_y_norm - E_norm.^2*sigma_v^2)./(1+sigma_v^2);
#k_norm = Var_x_norm./Var_y_norm ;
#k_norm(k_norm<0)=0;
#CFAR_lee = CFAR(k_norm);
#threshold = mean(CFAR_lee);
#%% Local values with mask
#Mask = maskDetect(Im, n, functionMode);
#Mask(k_norm<threshold) = -1;  % sets k value under threshold to have no mask % visualiser masker efter dette
#E = localMeanMask(Im, n, Mask);
#Var_y = localVarianceMask(Im,E, n,Mask);
#%% Weight 
#Var_x = (Var_y - E.^2*sigma_v^2)./(1+sigma_v^2);
#k = Var_x./Var_y ;
#k(k<0)=0;
#%% Creates the new picture
#Im = E + k.*(Im-E);
#
#
#
#
#
#
#
#
#
#
#
#
#def lee_filter(img, win_size=3, cu=CU_DEFAULT):
#    """
#    Apply lee to a numpy matrix containing the image, with a window of
#    win_size x win_size.
#    """
#    assert_window_size(win_size)
#
#    # we process the entire img as float64 to avoid type overflow error
#    img = np.float64(img)
#    img_filtered = np.zeros_like(img)
#    N, M = img.shape
#    win_offset = win_size / 2
#
#    for i in range(0, N):
#        xleft = i - win_offset
#        xright = i + win_offset
#
#        if xleft < 0:
#            xleft = 0
#        if xright >= N:
#            xright = N
#
#        for j in range(0, M):
#            yup = j - win_offset
#            ydown = j + win_offset
#
#            if yup < 0:
#                yup = 0
#            if ydown >= M:
#                ydown = M
#
#            assert_indices_in_range(N, M, xleft, xright, yup, ydown)
#
#            pix_value = img[i, j]
#            window = img[xleft:xright, yup:ydown]
#            w_t = weighting(window, cu)
#            window_mean = window.mean()
#            new_pix_value = (pix_value * w_t) + (window_mean * (1.0 - w_t))
#
#            assert new_pix_value >= 0.0, \
#                    "ERROR: lee_filter(), pixel filtered can't be negative"
#
#            img_filtered[i, j] = round(new_pix_value)
#
#    return img_filtered
######################################################################################
#
#
#
######################################################################################
## Enhanced Lee
#
#def weighting(pix_value, window, k=K_DEFAULT,
#              cu=CU_DEFAULT, cmax=CMAX_DEFAULT):
#    """
#    Computes the weighthing function for Lee filter using cu as the noise
#    coefficient.
#    """
#
#    # cu is the noise variation coefficient
#
#    # ci is the variation coefficient in the window
#    window_mean = window.mean()
#    window_std = window.std()
#    ci = window_std / window_mean
#
#    if ci <= cu:  # use the mean value
#        w_t = 1.0
#    elif cu < ci < cmax:  # use the filter
#        w_t = exp((-k * (ci - cu)) / (cmax - ci))
#    elif ci >= cmax:  # preserve the original value
#        w_t = 0.0
#
#    return w_t
#
#
#def assert_parameters(k, cu, cmax):
#    """
#    Asserts parameters in range.
#    Parameters:
#        - k: in [0:10]
#        - cu: positive
#        - cmax: positive and greater equal than cu
#    """
#
#    assert 0 <= k <= 10, \
#        "k parameter out of range 0<= k <= 10, submitted %s" % k
#
#    assert cu >= 0, \
#        "cu can't be negative"
#
#    assert cmax >= 0 and cmax >= cu, \
#        "cmax must be positive and greater equal to cu: %s" % cu
#
#
#def lee_enhanced_filter(img, win_size=3, k=K_DEFAULT, cu=CU_DEFAULT,
#                        cmax=CMAX_DEFAULT):
#    """
#    Apply Enhanced Lee filter to a numpy matrix containing the image, with a
#    window of win_size x win_size.
#    """
#    assert_window_size(win_size)
#    assert_parameters(k, cu, cmax)
#
#    # we process the entire img as float64 to avoid type overflow error
#    img = np.float64(img)
#    img_filtered = np.zeros_like(img)
#    N, M = img.shape
#    win_offset = win_size / 2
#
#    for i in xrange(0, N):
#        xleft = i - win_offset
#        xright = i + win_offset
#
#        if xleft < 0:
#            xleft = 0
#        if xright >= N:
#            xright = N
#
#        for j in xrange(0, M):
#            yup = j - win_offset
#            ydown = j + win_offset
#
#            if yup < 0:
#                yup = 0
#            if ydown >= M:
#                ydown = M
#
#            assert_indices_in_range(N, M, xleft, xright, yup, ydown)
#
#            pix_value = img[i, j]
#            window = img[xleft:xright, yup:ydown]
#            w_t = weighting(pix_value, window, k, cu, cmax)
#            window_mean = window.mean()
#
#            new_pix_value = (window_mean * w_t) + (pix_value * (1.0 - w_t))
#
#            assert new_pix_value >= 0.0, \
#                    "ERROR: lee_enhanced_filter(), pix " \
#                    "filter can't be negative"
#
#            img_filtered[i, j] = round(new_pix_value)
#
#    return img_filtered