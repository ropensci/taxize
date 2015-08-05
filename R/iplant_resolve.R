#' iPlant name resolution
#'
#' @export
#' @param query Vector of one or more taxonomic names. (no common names)
#' @param retrieve Specifies whether to retrieve all matches for the names submitted. One of 'best'
#' (retrieves only the single best match for each name submitted) or 'all' (retrieves all matches)
#' @param callopts Curl options passed on to \code{httr::GET}
#' @return A data frame
#' @examples \dontrun{
#' iplant_resolve(query=c("Helianthus annuus", "Homo sapiens"))
#' iplant_resolve("Helianthusss")
#' iplant_resolve("Pooa")
#'
#' library("httr")
#' iplant_resolve("Helianthusss", callopts=verbose())
#' }
#' @examples \dontrun{
#' # You can use the timeout function from httr to set a timeout for a given number of seconds
#' iplant_resolve("Helianthusss", callopts=timeout(seconds = 0.1))
#' }

iplant_resolve <- function(query, retrieve='all', callopts=list()){
  url <- "http://tnrs.iplantc.org/tnrsm-svc/matchNames"
  query <- paste(query, collapse = ",")
  args <- tc(list(names=query, retrieve=retrieve))
  out <- GET(url, query=args, callopts)
  warn_for_status(out)
  tt <- content(out, as = "text")
  res <- jsonlite::fromJSON(tt, FALSE)$items
  res2 <- do.call(rbind, lapply(res, data.frame, stringsAsFactors = FALSE))
  return( res2 )
}
