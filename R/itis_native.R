#' Get jurisdiction data, i.e., native or not native in a region.
#' 
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
#' 
#' # suppress message
#' itis_native(c(180543,41074,36616), verbose=FALSE)
#' }
#' @export 

itis_native <- function(tsn=NULL, what="bytsn", ...)
{
  temp <- switch(what, 
         bytsn = lapply(tsn, function(x) getjurisdictionaloriginfromtsn(x, ...)),
         values = getjurisdictionvalues(...),
         originvalues = getjurisdictionoriginvalues(...))
  if(length(tsn)==1){
    temp[[1]]
  } else
  {
    names(temp) <- tsn
    temp
  }
}