#' A Logit transformation can be applied to a variable with 
#' a known lower and upper limit (eg., p=0-1, 0-100%)
logit.transform <- function(x, direction= "forward", keep.na = FALSE) {
  if(max(x, na.rm = TRUE) > 1 { 
    if (min(x, na.rm = TRUE) < 0 || max(x, na.rm = TRUE) > 100) 
      stop("x must be in the range 0 to 100")
	x <- x / 100  
  }
    if (min(x, na.rm = TRUE) < 0 || max(x, na.rm = TRUE) > 1) 
      stop("x must be in the range 0 to 1")
  if(direction == "forward") {
  
    l <- log((x[!is.na(x)] / 100)/(1 - (x[!is.na(x)] / 100)))
	
  ifelse(direction == "back") {
   l <- exp(x[!is.na(x)])/(1 + exp(x[!is.na(x)])) * 100
  } else { stop("Not a valid option for direction") }
  return (l )  
}



log((0.5 + a * (x[!is.na(x)] - 0.5)) /
   (1 - (0.5 + a * (x[!is.na(x)] - 0.5))))
