#' Get TSN from LSID
#'
#' @export
#' @param lsid One or more lsid's
#' @param what What to retrieve. One of tsn, record, or fullrecord
#' @param ... Further arguments passed on to [ritis::lsid2tsn()],
#' [ritis::record()], or [ritis::full_record()]
#' @examples \dontrun{
#' # Get TSN
#' itis_lsid("urn:lsid:itis.gov:itis_tsn:180543")
#' itis_lsid(lsid=c("urn:lsid:itis.gov:itis_tsn:180543","urn:lsid:itis.gov:itis_tsn:28726"))
#'
#' # Get partial record
#' itis_lsid("urn:lsid:itis.gov:itis_tsn:180543", "record")
#'
#' # Get full record
#' itis_lsid("urn:lsid:itis.gov:itis_tsn:180543", "fullrecord")
#'
#' # An invalid lsid (a tsn actually)
#' itis_lsid(202385)
#' }
itis_lsid <- function(lsid=NULL, what='tsn', ...) {
  temp <- switch(
    what,
    tsn = lapply(lsid, function(x) ritis::lsid2tsn(x, ...)),
    record = lapply(lsid, function(x) ritis::record(x, ...)),
    fullrecord = lapply(lsid, function(x) ritis::full_record(lsid = x, ...))
  )
  if (length(lsid) == 1) {
    temp[[1]]
  } else {
    setNames(temp, lsid)
  }
}
