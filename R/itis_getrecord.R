#' Get full ITIS record for one or more ITIS TSN's or lsid's.
#' 
#' @param values One or more TSN's (taxonomic serial number) or lsid's for a 
#'    taxonomic group (character)
#' @param by By "tsn" or "lsid"
#' @param ... Further arguments passed on to getpublicationsfromtsn
#' @details You can only enter values in tsn parameter or lsid, not both.
#' @examples \dontrun{
#' # by TSN
#' itis_getrecord(202385)
#' itis_getrecord(c(202385,70340))
#' 
#' # by lsid
#' itis_getrecord("urn:lsid:itis.gov:itis_tsn:180543", "lsid")
#' 
#' # suppress message
#' itis_getrecord(202385, verbose=FALSE)
#' }
#' @export 

itis_getrecord <- function(values=NULL, by="tsn", ...)
{
  if(is.null(values))
    stop("You must provide values")
  by <- match.arg(by, choices=c("tsn","lsid"))
  
  temp <- switch(by, 
         tsn = lapply(values, function(x) getfullrecordfromtsn(x, ...)),
         lsid = lapply(values, function(x) getfullrecordfromlsid(x, ...)))
  if(length(values)==1){
    temp[[1]]
  } else
  {
    names(temp) <- values
    temp
  }
}