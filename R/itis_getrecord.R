#' Get full ITIS record for one or more ITIS TSN's or lsid's.
#'
#' @export
#' @param values (character) One or more TSN's (taxonomic serial number) or lsid's for a
#' taxonomic group
#' @param by (character) By "tsn" (default) or "lsid"
#' @param ... Further arguments passed on to \code{\link[ritis]{full_record}}
#' @details You can only enter values in tsn parameter or lsid, not both.
#' @examples \dontrun{
#' # by TSN
#' itis_getrecord(202385)
#' itis_getrecord(c(202385,70340))
#'
#' # by lsid
#' itis_getrecord("urn:lsid:itis.gov:itis_tsn:202385", "lsid")
#' }

itis_getrecord <- function(values, by="tsn", ...) {
  if (!by %in% c("tsn","lsid")) stop("by must be one of 'tsn' or 'lsid'", call. = FALSE)
  temp <- switch(
    by,
    tsn = lapply(values, ritis::full_record, ...),
    lsid = lapply(values, function(x) ritis::full_record(lsid = x, ...))
  )
  if (length(values) == 1) {
    temp[[1]]
  } else {
    setNames(temp, values)
  }
}
