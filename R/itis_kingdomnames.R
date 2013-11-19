#' Get kingdom names.
#' 
#' @param tsn One or more TSN's (taxonomic serial number)
#' @param ... Further arguments passed on to getkingdomnamefromtsn
#' @examples \dontrun{
#' itis_kingdomnames(202385)
#' itis_kingdomnames(tsn=c(202385,183833,180543))
#' 
#' # suppress message
#' itis_kingdomnames(c(202385,183833,180543), verbose=FALSE)
#' }
#' @export 

itis_kingdomnames <- function(tsn=NULL, ...)
{
  if(is.null(tsn)){ getkingdomnames() } else
  { sapply(tsn, function(z) as.character(getkingdomnamefromtsn(z, ...)$kingdomName)) }
}