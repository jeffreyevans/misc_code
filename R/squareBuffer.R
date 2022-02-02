#' @examples
#' library(sf)
#' xy <- st_as_sf(data.frame(x = c(1,3,6,7),
#'                y = c(3,2,7,8), z = c(38,23,12,12),
#'                area = c(32,23,45,67)),
#' 			   coords = c("x", "y"), 
#' 			   agr = "constant") 
#' 
#' # With variable buffer
#' sb.var <- squareBuffer(xy, xy$area)
#'   plot(st_geometry(sb.var))
#'     plot(st_geometry(xy), pch=20, add=TRUE)
#'   
#' # With fixed buffer
#' sb <- squareBuffer(xy, 32)
#'   plot(st_geometry(sb))
#'     plot(st_geometry(xy), pch=20, add=TRUE)
#'  
#' @export				 					   
squareBuffer <- function(x, a) {
  if(!any(class(x) == "sf"))
    stop("x needs to be an sf object")
  a <- sqrt(a)/2
return( sf::st_buffer(x, dist = a, nQuadSegs=1, 
                      endCapStyle = "SQUARE") )
}
