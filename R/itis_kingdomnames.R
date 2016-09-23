#' Get kingdom names
#'
#' @export
#' @param tsn One or more TSN's (taxonomic serial number)
#' @param ... Further arguments passed on to getkingdomnamefromtsn
#' @examples \dontrun{
#' itis_kingdomnames(202385)
#' itis_kingdomnames(tsn=c(202385,183833,180543))
#' }
itis_kingdomnames <- function(tsn=NULL, ...) {
  if (is.null(tsn)) {
    ritis::kingdom_names(...)
  } else {
    sapply(
      tsn,
      function(z) {
        suppressWarnings(
          as.character(ritis::kingdom_name(z, ...)$kingdomname)
        )
      }
    )
  }
}
