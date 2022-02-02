#' @title Calculate fragmentation indices for a raster
#'
#' @description
#' The function calculates a set of fragmentation indices as per 
#' 
#' @param rast Raster with binary values (1 or 0 or \code{NA}).
#' @param size Integer, number of cells wide and high of the window used to calculate 
#'        fragmentation. This must be an odd integer (default is 3).
#' @param pad Logical, if \code{TRUE} then add virtual rows and columns around the raster 
#'        so that there are no edge effects. The virtual rows and columns are set to equal 
#'        \code{padValue}. Default is \code{FALSE}.
#' @param padValue Value to which to set the values of the virtual cells used to pad the 
#'        raster if \code{pad} is \code{TRUE}.
#' @param calcDensity Logical, if \code{TRUE} (default) then calculate density raster.
#' @param calcConnect Logical, if \code{TRUE} (default) then calculate connectivity raster.
#' @param calcConnect Logical, if \code{TRUE} (default) then calculate classification 
#'        raster. Note that to calculate the classification raster the density and connectivity rasters must also be calculated 
#'       (\code{calcDensity} and \code{calcConnect} should both be \code{TRUE}). If they are 
#'       not then the will be forced to \code{TRUE} with a warning.
#' @param na.rm Logical, if \code{FALSE} (default) then \code{NA} cells count as part of 
#'        the area potentially occupied in a window (i.e., the count in the denominator when 
#'        calculating density and they are counted as potential links in the connectance 
#'        calculations if a neighboring cell has a value of 1). If \code{FALSE} then areas 
#'        that border \code{NA} cells could still be classified as "interior" or otherwise 
#'        have less apparent fragmentation if the occupied cells are fully surrounded by other 
#'        occupied cells (except for the \code{NA} cells).
#' @param undet Character. When classifying this defines what is done with "undetermined" 
#'        cases (when density is >= 0.6 and density == connectivity). Possible values include 
#' \itemize{
#' 	\item \code{undetermined}: Undetermined cases will be assigned a value of 5 (which is not assigned to any other case; default).
#' 	\item \code{perforated}: Undetermined cases will be assigned a value of 3 ("perforated").
#' 	\item \code{edge}: Undetermined cases will be assigned a value of 4 ("edge").
#' 	\item \code{random}: Undetermined cases will be assigned a value of 3 or 4 at random ("perforated" or "edge").
#' }
#' @param ... Other arguments (not used).
#' 
#' @return A raster stack with four rasters: a fragmentation classification 
#' (named \code{class}), the density of "1" pixels in the window (named \code{density} 
#'  called "pf" in Riitter et al. 2000), and a connectivity raster (conditional probability 
#'  a cell with a value of 1 has a value that is also 1; named \code{connect}--called "pff" 
#'  in Riitter et al. 2000).
#'
#' @note
#' The density and connectivity rasters have values in the range [0, 1], but the 
#' classification raster has coded values (from the erratum to Ritter et al. (2000):
#' \itemize{
#' 	\item \code{NA}: \code{NA}
#' 	\item \code{0}: No forest (or whatever is being evaluated)
#'	\item \code{1}: patch (\code{pf} < 0.4)
#'	\item \code{2}: transitional (0.4 <= \code{pf} < 0.6)
#'	\item \code{3}: perforated (\code{pf} >= 0.6 & \code{pf - pff} > 0)
#'	\item \code{4}: edge (\code{pf} >= 0.6 & \code{pf - pff} < 0)
#'	\item \code{5}: undetermined (\code{pf} >= 0.6 & \code{pf == pff})
#'	\item \code{6}: interior (\code{pf} == 1)
#' }
#' Note that this differs somewhat from the numbering scheme presented by
#' Riitters et al. (2000), see the erratum to the paper on their classification 
#' scheme at <https://www.ecologyandsociety.org/vol4/iss2/art3/errata/january26.2001.html>). 
#' 
#' @references
#' Riitters, K., J. Wickham, R. O'Neill, B. Jones, and E. Smith. 2000. Global-scale patterns of 
#'   forest fragmentation. Conservation Ecology 4:3. URL: <https://www.jstor.org/stable/26271763>.
#'
#' @examples
#' \donttest{
#' library(raster)
#' library(terra)
#' 
#' fnf <- raster(nrows=180, ncols=360, xmn=571823.6, xmx=616763.6, ymn=4423540, 
#'             ymx=4453690, resolution=270, crs = CRS("+proj=utm +zone=12 +datum=NAD83 
#'             +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
#'   fnf[] <- rpois(ncell(fnf), lambda=1)
#'     fnf <- calc(fnf, fun=function(x) { x[x >= 1] <- 1; return(x) } )  
#'   
#'  # Run as raster
#'    system.time({ 
#'      fragmentation(fnf, size=5, pad=TRUE, undet='perforated')
#'    })
#'    
#'  # Run as terra
#'    fnf <- terra::rast(fnf) 
#'    system.time({   
#'      frag <- fragmentation(fnf, size=5, pad=TRUE, undet='perforated')
#'    })
#'  
#'   par(mfrow=c(2, 2))
#'     plot(fnf, col=c('gray90', 'forestgreen'), main='Forest Cover')
#'     plot(frag[['density']], main='Density in 2000')
#'     plot(frag[['connect']], main='Connectivity in 2000')
#'     cols <- c('gray90', 'blue', 'lightblue', 'yellow', 'orange', 'forestgreen')
#'       names(cols) <- c('no forest', 'patch', 'transitional',
#'   		              'perforated', 'edge', 'interior')
#'     plot(frag[['class']], main='Fragmentation Class', col=cols, legend=FALSE)
#'       legend('topright', fill=cols, legend=names(cols), bg="white") 
#'  }
#'    
#'  library(landscapemetrics)
#'  library(raster)
#'  library(terra)
#'    data(landscape)
#'    fnf <- rast(landscape)
#'  
#'  fnf <- ifel(fnf > 1, 1, 0)
#'    frag <- fragmentation(fnf, size=5, pad=TRUE, undet='perforated')
#'  
#'  plot(fnf, col=c('gray90', 'forestgreen'), main='Forest Cover')	              
#'  dev.new()
#'  cols <- c('gray90', 'blue', 'lightblue', 'yellow', 'orange', 'forestgreen')
#'    names(cols) <- c('no forest', 'patch', 'transitional',
#'                     'perforated', 'edge', 'interior')
#'  plot(frag[['class']], main='Fragmentation Class', col=cols, legend=FALSE)
#'    legend('topright', fill=cols, legend=names(cols), bg="white") 
#'  
#'
#' @seealso \code{\link[raster]{focal}}
#' @seealso \code{\link[terra]{focal}}
#'
#' @export
fragmentation <- function (x, size = 3, pad = FALSE, padValue = NA,
                           undet = c("undetermined", "perforated", "edge", "random"), 
                           calcDensity = TRUE, calcConnect = TRUE, 
						   calcClass = TRUE, na.rm = FALSE, ...) {
	if(!any(class(x)[1] %in% c("RasterLayer", "SpatRaster")))
      stop("x must be a RasterLayer (raster) or SpatRaster (terra) object")	
	rclass = class(x)					   
    if (size%%2 == 0 | size < 3) 
        stop("Argument \"size\" to function fragmentation() must be an odd integer >= 3.")
    if (calcClass & (!calcDensity | !calcConnect)) {
        calcDensity <- calcConnect <- TRUE
        warning("Forcing \"calcDensity\" and \"calcConnect\" in function \"fragmentation()\" to be TRUE since \"calcClass\" is TRUE.")
    }
	undet = undet[1]
    halfWindowSize <- (size - 1)/2
    w <- matrix(1, nrow = size, ncol = size)    
	if (pad) {
	  if(class(x)[1] == "RasterLayer") {
        origExtent <- raster::extent(x)
        x <- raster::extend(x, y = halfWindowSize, value = padValue)
	  } else if(class(x)[1] == "SpatRaster") {
        origExtent <- terra::ext(x)
        x <- terra::extend(x, y = halfWindowSize)
      }	   
    }
	if (calcDensity) {
      if(class(x)[1] == "RasterLayer") {
        pf <- raster::focal(x, w = w, fun = fDensity,  na.rm = na.rm)
	  } else if(class(x)[1] == "SpatRaster") {
        pf <- terra::focal(x, w = w, fun = fDensity,  na.rm = na.rm)
      }	   
    }
	if (calcConnect){ 
      if(class(x)[1] == "RasterLayer") {
        pff <- raster::focal(x, w = w, fun = fConnect, na.rm = na.rm)
	  } else if(class(x)[1] == "SpatRaster") {
        pff <- terra::focal(x, w = w, fun = fConnect, na.rm = na.rm)
      }
	}
	if(calcClass) {
      if(class(x)[1] == "RasterLayer") {
        out <- raster::stack(pf, pff)
          names(out) <- c("density", "connect")
        classification <- raster::calc(out, fun = fClassify, undet = undet)
          names(classification) <- "class"
            out <- raster::stack(classification, out)
		raster::crs(out) <- raster::crs(x)	
	  } else if(class(x)[1] == "SpatRaster") {
        out <- c(pf, pff)
          names(out) <- c("density", "connect")
        classification <- terra::app(out, fun = fClassify, undet = undet)
          names(classification) <- "class"
            out <- c(classification, out)
        terra::crs(out) <- terra::crs(x)
      }
    }
	if (pad) {
      if(class(x)[1] == "RasterLayer") {	
        out <- raster::crop(out, origExtent)
	  } else if(class(x)[1] == "SpatRaster") {	  
        out <- terra::crop(out, origExtent)	  
	  }
	}
  return(out)
}

fDensity <- function (x, na.rm = FALSE) {
    if (all(is.na(x))) return(NA)  
    x <- matrix(x, nrow = round(sqrt(length(x))), byrow = FALSE)
      n <- if (na.rm) { sum(!is.na(x)) }
    else {
      nrow(x) * ncol(x)
    }
      occ <- if (n == 0) { 0 }
    else {
      sum(x, na.rm = TRUE)
    }
    return( occ / n )
} 

fConnect <- function (x, na.rm = FALSE) {
    if (all(is.na(x))) 
        return(NA)
    x <- matrix(x, nrow = round(sqrt(length(x))), byrow = FALSE)
      xUpper <- x[1:(nrow(x) - 1), ]
        xLower <- x[2:nrow(x), ]
          xLeft <- x[, 1:(ncol(x) - 1)]
          xRight <- x[, 2:ncol(x)]
        rowsAdj <- xUpper == xLower & xUpper == 1 & xLower == 1
      colsAdj <- xLeft == xRight & xLeft == 1 & xRight == 1
    adj <- sum(rowsAdj, colsAdj, na.rm = TRUE)
	rowsPoss <- (xUpper == 1 | xLower == 1) & !is.na(xUpper) & !is.na(xLower)
    colsPoss <- (xLeft == 1 | xRight == 1) & !is.na(xLeft) & !is.na(xRight)
    if (na.rm) {
      rowsPoss <- rowsPoss & !is.na(xUpper) & !is.na(xLower)
      colsPoss <- colsPoss & !is.na(xLeft) & !is.na(xRight)
    }
      possible <- sum(rowsPoss, colsPoss)
      connect <- if (possible == 0) { 0 }
    else {
      connect <- adj / possible
    }
  return( connect )
}

fClassify <- function (x, undet = "undetermined", ...) {
  ftype <- c("undetermined", "perforated", "edge", "random")  
    pf <- x[1]
    pff <- x[2]
    fr <- ifelse(anyNA(x), NA, 
      ifelse(pf == 0, 0, 
        ifelse(pf > 1 - .Machine$double.eps, 6, 
          ifelse(pf < 0.4, 1, 
            ifelse(pf >= 0.4 & pf < 0.6, 2,
             ifelse(pf >= 0.6 & pf > pff, 3,
            ifelse(pf >= 0.6 & pf == pff & pmatch(undet, ftype) == 1, 5,  
        ifelse(pf >= 0.6 & pf == pff & pmatch(undet, ftype) == 2, 3,  
      ifelse(pf >= 0.6 & pf == pff & pmatch(undet, ftype) == 3, 4,  
    ifelse(pf >= 0.6 & pf == pff & pmatch(undet, ftype) == 4, 
           sample(c(3, 4), 1), NA))))))))))  
  return(fr)
}    

# fClassify <- function (x, undet = "undetermined", ...) {
#   # (1) interior, for which Pf = 1.0; 
#   # (2) patch, Pf < 0.4; 
#   # (3) transitional, 0.4 < Pf < 0.6; 
#   # (4) perforated, Pf > 0.6 and Pf - Pff > 0; 
#   # (5) edge, Pf > 0.6 and Pf – Pff < 0, and 
#   # (6) undetermined, Pf > 0.6 and Pf = Pff. 
#   #   (1) edge, if Pf > 0.6 and Pf - Pff < 0
#   #   (2) undetermined, if Pf > 0.6 and Pf = Pff
#   #   (3) perforated, if Pf > 0.6 and Pf - Pff > 0
#   #   (4) interior, if Pf = 1.0
#   #   (5) patch, if Pf < 0.4
#   #   (6) transitional, if 0.4 < Pf < 0.6 
#   frag.type <- c("undetermined", "perforated", "edge", "random")  
#     pf <- x[1]
#     pff <- x[2]
#     if (anyNA(x)) {
#         NA
#     } else if (pf == 0) { 0
#       } else if (pf > 1 - .Machine$double.eps) { 6
#         } else if (pf < 0.4) { 1
#           } else if (pf >= 0.4 & pf < 0.6) { 2
#         } else if (pf >= 0.6 & pf > pff) { 3
#       } else if (pf >= 0.6 & pf < pff) { 4
#  } else if (pf >= 0.6 & pf == pff) {
#       if (pmatch(undet, frag.type) == 1) { 5
#         } else if (pmatch(undet, frag.type) == 2) { 3
#           } else if (pmatch(undet, frag.type)) == 3) { 4
#             } else if (pmatch(undet, frag.type) == 4) {
#               sample(c(3, 4), 1)
#         }
# 		
#       }
# } 
