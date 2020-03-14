#' Get jurisdiction data, i.e., native or not native in a region.
#'
#' @export
#' @param tsn One or more TSN's (taxonomic serial number)
#' @param what One of bytsn, values, or originvalues
#' @param ... Further arguments passed on to
#' [ritis::jurisdictional_origin()],
#' [ritis::jurisdiction_values()], or
#' [ritis::jurisdiction_origin_values()]
#' @examples \dontrun{
#' # Get values
#' itis_native(what="values")
#'
#' # Get origin values
#' itis_native(what="originvalues")
#'
#' # Get values by tsn
#' itis_native(tsn=180543)
#' itis_native(tsn=c(180543,41074,36616))
#' }

itis_native <- function(tsn=NULL, what="bytsn", ...) {
  switch(
    what,
    bytsn = {
      temp <- lapply(tsn, function(x) ritis::jurisdictional_origin(x, ...))
      if (length(tsn) == 1) {
        temp[[1]]
      } else {
        setNames(temp, tsn)
      }
    },
    values = ritis::jurisdiction_values(...),
    originvalues = ritis::jurisdiction_origin_values(...))
}
