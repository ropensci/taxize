#' Get references related to a ITIS TSN.
#'
#' @export
#' @param tsn One or more TSN's (taxonomic serial number) for a taxonomic group (numeric)
#' @param ... Further arguments passed on to getpublicationsfromtsn
#' @examples \dontrun{
#' itis_refs(202385)
#' itis_refs(c(202385, 70340))
#' }
itis_refs <- function(tsn, ...) {
  lapply(tsn, ritis::publications, ...)
}
