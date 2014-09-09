#' Search the CANADENSYS Vascan API.
#' 
#' For more information, see \url{http://data.canadensys.net/vascan/search}.
#' 
#' @import httr jsonlite
#' @export
#' 
#' @param q (character) Can be a scientific name, a vernacular name or a VASCAN
#'    taxon identifier (e.g. 861)
#' @param format (character) One of json (default) or xml.
#' @param raw (logical) If TRUE, raw json or xml returned, if FALSE, parsed data returned.
#' @param callopts (list) Further args passed on to htt::GET.
#' @author Scott Chamberlain {myrmecocystus@@gmail.com}
#' @return json, xml or a list.
#' @references API docs \url{http://data.canadensys.net/vascan/api}. You can also
#' download bulk data \url{http://data.canadensys.net/ipt/resource.do?r=vascan&request_locale=en}
#' @keywords names taxonomy
#' @examples \donttest{
#' vascan_search(q = "Helianthus annuus")
#' vascan_search(q = "Helianthus annuus", raw=TRUE)
#' vascan_search(q = c("Helianthus annuus", "Crataegus dodgei"), raw=TRUE)
#' 
#' # format type
#' ## json
#' c <- vascan_search(q = "Helianthus annuus", format="json", raw=TRUE)
#' library("jsonlite")
#' fromJSON(c, FALSE)
#' 
#' ## xml
#' d <- vascan_search(q = "Helianthus annuus", format="xml", raw=TRUE)
#' library("XML")
#' xmlParse(d)
#' 
#' # lots of names, in this case 50
#' splist <- names_list(rank='species', size=50)
#' vascan_search(q = splist)
#' }
vascan_search <- function(q, format='json', raw=FALSE, callopts=list())
{
  url <- sprintf("http://data.canadensys.net/vascan/api/0.1/search.%s", format)
  if(!length(q) > 1){
    args <- list(q=q)
    tt <- GET(url, query=args, callopts)
    stop_for_status(tt)
    out <- content(tt, as="text")
  } else
  {
    args <- paste(q, collapse='\n')
    tt <- POST(url, body=list(q=args), encode='form')
    stop_for_status(tt)
    out <- content(tt, as="text")
  }
  if(raw){ return( out ) } else { 
    tmp <- jsonlite::fromJSON(out, FALSE) 
    res <- tmp$results[[1]]
    lapply(tmp$results, vascan_parse)
  }
}

vascan_parse <- function(x){
  parsed <- lapply(x$matches, function(x){
    taxass <- data.frame(x$taxonomicAssertions, stringsAsFactors = FALSE)
    dist <- data.frame(rbindlist(x$distribution))
    vern <- data.frame(rbindlist(x$vernacularNames))
    list(taxonomicassertions=taxass, distribution=dist, vernacularnames=vern)
  })
  if(length(parsed) == 0){ 
    data.frame(searchedterm=x$searchedTerm, nummatches=x$numMatches, matches=NA)
  } else {
    list(searchedterm=x$searchedTerm, nummatches=x$numMatches, matches=parsed)
  }
}
