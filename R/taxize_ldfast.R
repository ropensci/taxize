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
    dt2df(lapply(x, convert2df), idcol = FALSE)
  } else {
    dt2df(x, idcol = FALSE)
  }
}
