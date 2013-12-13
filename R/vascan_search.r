#' Search the CANADENSYS Vascan API.
#' 
#' For more information, see \url{http://data.canadensys.net/vascan/search}.
#' 
#' @import httr 
#' @param q (character) Can be a scientific name, a vernacular name or a VASCAN
#'    taxon identifier (e.g. 861)
#' @param format (character) One of json (default) or xml.
#' @param raw (logical) If TRUE, raw json or xml returned, if FALSE, parsed data returned.
#' @param callopts (list) Further args passed on to htt::GET.
#' @author Scott Chamberlain {myrmecocystus@@gmail.com}
#' @return json, xml or a list.
#' @references API docs \url{http://data.canadensys.net/vascan/api}. You can also
#' download bulk data \url{http://data.canadensys.net/ipt/resource.do?r=vascan&request_locale=en}
#' @export
#' @keywords names taxonomy
#' @examples \dontrun{
#' vascan_search(q = "Helianthus annuus")
#' vascan_search(q = "Helianthus annuus", raw=TRUE)
#' vascan_search(q = c("Helianthus annuus", "Crataegus dodgei"), raw=TRUE)
#' 
#' # format type
#' ## json
#' c <- vascan_search(q = "Helianthus annuus", format="json", raw=TRUE)
#' library(rjson)
#' fromJSON(c)
#' 
#' ## xml
#' d <- vascan_search(q = "Helianthus annuus", format="xml", raw=TRUE)
#' library(XML)
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
    tt <- POST(url, body=list(q=args), multipart=FALSE)
    stop_for_status(tt)
    out <- content(tt, as="text")
  }
  if(raw){ return( out ) } else { fromJSON(out) }
}