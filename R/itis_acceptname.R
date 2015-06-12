#' Retrieve accepted TSN (with accepted name).
#'
#' @param searchtsn Quoted TSN for a taxonomic group (character).
#' @param ... Further arguments passed on to getacceptednamesfromtsn
#' @details You can print informative messages by setting supmess=FALSE.
#' @return Names or TSNs of all downstream taxa.
#' @export
#' @examples \dontrun{
#' itis_acceptname('208527')  # TSN accepted - good name
#' itis_acceptname('504239')  # TSN not accepted - input TSN is old
#' }
itis_acceptname <- function(searchtsn = NA, ...) {
	tt <- getacceptednamesfromtsn(searchtsn, ...)
	if (length(tt) == 1) {
	  tt
	} else {
    tmp <- data.frame(tt, stringsAsFactors = FALSE)
    setNames(tmp, tolower(names(tmp)))
	}
}
