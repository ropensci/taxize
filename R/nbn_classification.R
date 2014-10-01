#' Search UK National Biodiversity Network database for taxonomic classification
#' 
#' @export
#' 
#' @param q (character) The query terms(s)
#' @param prefered (logical) Restrict search to preferred or any
#' @param order (character) The order in which we should sort the results. Default: asc
#' @param sort (character) Sort the results or not.
#' @param start (integer/numeric) The page that the user wants to start displaying the results at. 
#' Default: 0
#' @param rows (integer/numeric) The number of rows to show in each page of search results. 
#' Default: 25
#' @param taxonOutputGroupKey (character) Vector of taxon output groups.
#' @param all (logical) Get all results, overrides rows parameter if TRUE. Default: FALSE
#' @param ... Further args passed on to \code{\link[httr]{GET}}. 
#' 
#' @author Scott Chamberlain, \email{myrmecocystus@@gmail.com}
#' 
#' @examples \donttest{
#' nbn_classifcation(id="NHMSYS0000502940")
#' 
#' # get id first, then pass to this fxn
#' id <- get_nbnid("blue tit", rec_only = TRUE, rank = "Species")
#' nbn_classifcation(id)
#' 
#' library('httr')
#' nbn_classifcation(id="NHMSYS0000502940", config=verbose())
#' }
nbn_classifcation <- function(id, ...)
{
  url <- sprintf("https://data.nbn.org.uk/api/taxa/%s/taxonomy", id)
  nbn_class_GET(url, ...)
}

nbn_class_GET <- function(url, ...){
  res <- GET(url, ...)
  stop_for_status(res)
  tt <- content(res, as = "text")
  jsonlite::fromJSON(tt, TRUE)
}
