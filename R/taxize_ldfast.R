#' Replacement function for ldply that should be faster in all cases.
#'
#' @param x A list.
#' @param convertvec Convert a vector to a data.frame before rbind is called.
#' @export
#' @keywords internal
taxize_ldfast <- function(x, convertvec=FALSE){
  convert2df <- function(x){
    if (!inherits(x, "data.frame")) {
      data.frame(rbind(x))
    } else {
      x
    }
  }

  if (convertvec) {
    do.call(rbind.fill, lapply(x, convert2df))
  } else {
    do.call(rbind.fill, x)
  }
}
