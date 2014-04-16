#' iplant resolution
#' 
#' @import httr RJSONIO
#' @export
#' @param query Vector of names
#' @param retrieve Specifies whether to retrieve all matches for the names submitted. One of 'best' 
#' (retrieves only the single best match for each name submitted) or 'all' (retrieves all matches)
#' @param callopts Curl options passed on to \code{httr::GET}
#' @examples \dontrun{
#' iplant_resolve(query=c("Helianthus annuus", "Homo sapiens"))
#' }

iplant_resolve <- function(query, retrieve='all', callopts=list()){
  url <- "http://tnrs.iplantc.org/tnrsm-svc/matchNames"
  query <- paste(query, collapse = ",")
  args <- compact(list(names=query, retrieve=retrieve))
  out <- GET(url, query=args, callopts)
  stop_for_status(out)
  tt <- content(out, as = "text")
  res <- RJSONIO::fromJSON(tt)$items
  return( res )
}