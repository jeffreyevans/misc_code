#' @title row bind unequal data
#' @description Applies rbind to unequal vectors to create a data.frame
#'
#' @param x   list object with named vectors or data.frames to rbind into data.frame 
#' @param ... not used 
#'
#' @return A data.frame with NA's where data do not match 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> and Tim Boucher <tboucher@@tnc.org>
#'
#' @examples
#' ul <- list()
#'   ul[[1]] <- runif(2)
#'     names(ul[[1]]) <- 1:2
#'   ul[[2]] <- runif(3)
#'     names(ul[[2]]) <- 1:3
#'   ul[[3]] <- runif(4)
#'     names(ul[[3]]) <- 1:4
#'   ul 
#' rbind.unequal(ul)
#'
#' @export rbind.unequal
rbind.unequal <- function(x, ...) {
  if(class(x) != "list") 
    stop("x must be a list")
  n <- sapply(x, names)
    un <- unique(unlist(n))
    len <- sapply(x, length)
    out <- vector("list", length(len))
    for (i in seq_along(len)) {
      out[[i]] <- unname(x[[i]])[match(un, n[[i]])]
    }
  return(stats::setNames(as.data.frame(do.call(rbind, out), 
         stringsAsFactors=FALSE), un))
}
