#' Point-Biserial correlation 
#'
#' Measures the strength and direction of the association 
#' between a continuous and binary variable
#'
#' @examples
#' library(randomForest)	
#' data(iris)
#'   iris <- na.omit(iris)
#'   y <- ifelse(iris$Species == "setosa" | 
#'                 iris$Species == "virginica", 1, 0) 
#'  
#' rf.mdl <- randomForest(iris[,1:4], as.factor(y), ntree=501) 
#'   rf.pred <- predict(rf.mdl, iris[,1:4], type="prob")[,2]
#' 
#'  biserial.cor(rf.pred, y)
#' 
biserial.cor <- function (x, y, class = 1) {
  if (!is.numeric(x)) stop("x must be numeric")
    y <- as.factor(y)
  if (length(levs <- levels(y)) > 2) 
	stop("y must be binary")
  if (length(x) != length(y)) 
	stop("x and y do not have the same length")
  ind <- y == levs[class]
    diff.mu <- mean(x[ind]) - mean(x[!ind])
    prob <- mean(ind)
  diff.mu * sqrt(prob * (1 - prob)) / sd(x)
}
