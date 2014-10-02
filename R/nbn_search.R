#' Search UK National Biodiversity Network database
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
#' nbn_search(q = "blackbird")
#' nbn_search(q = "blackbird", start = TRUE)
#' nbn_search(q = "blackbird", all = TRUE)
#' nbn_search(q = "blackbird", taxonOutputGroupKey = "NHMSYS0000080039")
#' 
#' # debug curl stuff
#' library('httr')
#' nbn_search(q = "blackbird", config = verbose())
#' }
nbn_search <- function(q, prefered = FALSE, order = 'asc', sort = NULL, start = 0,
  rows = 25, taxonOutputGroupKey = NULL, all = FALSE, ...)
{
  url <- "https://data.nbn.org.uk/api/search/taxa"
  args <- taxize_compact(list(q = q, prefered = prefered, order = order, sort = sort, start = start,
                              rows = rows, taxonOutputGroupKey = taxonOutputGroupKey))
  if(all){
    args$rows <- 0
    num <- nbn_GET(url, args)$meta$numFound
    args$rows <- num
    nbn_GET(url, args, ...)
  } else { nbn_GET(url, args, ...) }
}

nbn_GET <- function(url, args, ...){
  res <- GET(url, query = args, ...)
  stop_for_status(res)
  tt <- content(res, as = "text")
  json <- jsonlite::fromJSON(tt, FALSE)
  dat <- do.call(rbind.fill, lapply(json$results, data.frame, stringsAsFactors = FALSE))
  list(meta=data.frame(json$header, stringsAsFactors = FALSE), data=dat)
}
