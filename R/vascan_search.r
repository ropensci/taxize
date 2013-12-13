#' Search the CANADENSYS Vascan API
#' 
#' For more information, see \url{http://data.canadensys.net/vascan/search}.
#' 
#' @import httr
#' @importFrom RJSONIO toJSON
#' @param q (character) Can be a scientific name, a vernacular name or a VASCAN
#'    taxon identifier (e.g. 861)
#' @param format (character) One of json (default) or xml.
#' @param raw (logical) If TRUE, raw json or xml returned, if FALSE, parsed data returned.
#' @param callopts (list) Further args passed on to htt::GET.
#' @author Scott Chamberlain {myrmecocystus@@gmail.com}
#' @return json, xml or a data.frame.
#' @references API docs \url{http://data.canadensys.net/vascan/api}. You can also
#' download bulk data \url{http://data.canadensys.net/ipt/resource.do?r=vascan&request_locale=en}
#' @export
#' @keywords names taxonomy
#' @examples \dontrun{
#' vascan_search(q = "Helianthus annuus")
#' vascan_search(q = "Helianthus annuus", raw=TRUE)
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
    stop("post not working yet")
#     args <- paste(q, collapse='\n')
#     tt <- 
#       POST(url, body=upload_file(path="tab.txt"))
#     stop_for_status(tt)
#     out <- content(tt)
  }
  if(raw){ return( out ) } else { parse_vascan(out) }
}

parse_vascan <- function(x){
  toJSON(x)
}