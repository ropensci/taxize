#' Get jurisdiction data, i.e., native or not native in a region.
#'
#' @export
#' @param tsn One or more TSN's (taxonomic serial number)
#' @param what One of bytsn, values, or originvalues
#' @param ... Further arguments passed on to \code{\link{getjurisdictionaloriginfromtsn}}
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
  switch(what,
         bytsn = {
           temp <- lapply(tsn, function(x) getjurisdictionaloriginfromtsn(x, ...))
           if (length(tsn) == 1) {
             temp[[1]]
           } else {
             setNames(temp, tsn)
           }
         },
         values = getjurisdictionvalues(...),
         originvalues = getjurisdictionoriginvalues(...))
}
