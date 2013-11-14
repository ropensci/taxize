#' Get hierarchies from TSN values, full, upstream only, or immediate downstream 
#' only
#' 
#' @param tsn One or more TSN's (taxonomic serial number)
#' @param what One of full (full hierarchy), up (immediate upstream), or down 
#'    (immediate downstream)
#' @param ... Further arguments passed on to \code{\link{getjurisdictionaloriginfromtsn}}
#' @seealso \code{\link{itis_downstream}}
#' @details Note that \code{\link{itis_downstream}} gets taxa downstream to a particular
#'    rank, whilc this function only gets immediate names downstream.
#' @examples \dontrun{
#' # Get full hierarchy
#' itis_hierarchy(tsn=180543)
#' 
#' # Get hierarchy upstream
#' itis_hierarchy(tsn=180543, "up")
#' 
#' # Get hierarchy downstream
#' itis_hierarchy(tsn=180543, "down")
#' 
#' # Many tsn's
#' itis_hierarchy(tsn=c(180543,41074,36616))
#' 
#' # Suppress messages
#' itis_hierarchy(c(180543,41074,36616), verbose=FALSE)
#' }
#' @export 

itis_hierarchy <- function(tsn=NULL, what="full", ...)
{
  if(is.null(tsn))
    stop('You must supply one or more values in tsn parameter')
  
  temp <- switch(what,
         full = lapply(tsn, function(x) getfullhierarchyfromtsn(x, ...)),
         up = lapply(tsn, function(x) gethierarchyupfromtsn(x, ...)),
         down = lapply(tsn, function(x) gethierarchydownfromtsn(x, ...)))
  if(length(tsn)==1){
    temp[[1]]
  } else
  {
    names(temp) <- tsn
    temp
  }
}