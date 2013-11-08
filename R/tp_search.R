#' Search Tropicos by scientific name, common name, or Tropicos ID.
#' 
#' @import httr plyr
#' @param name Your search string. For instance "poa annua"
#' @param commonname Your search string. For instance "annual blue grass"
#' @param nameid Your search string. For instance "25509881"
#' @param orderby Your search string. For instance "1"
#' @param sortorder Your search string. For instance "ascending"
#' @param pagesize Your search string. For instance "100"
#' @param startrow Your search string. For instance "1"
#' @param type Type of search, "wildcard" (default) will add a wildcard to the end
#'    of your search string. "exact" will use your search string exactly.
#' @param key Your Tropicos API key; loads from .Rprofile.
#' @param callopts Further args passed on to httr::GET
#' @return List or dataframe.
#' @references \url{http://services.tropicos.org/help?method=SearchNameXml}
#' @export
#' @examples \dontrun{
#' tp_search(name = 'Poa annua')
#' }
tp_search <- function(name=NULL, commonname=NULL, nameid=NULL, orderby=NULL, 
  sortorder=NULL, pagesize=NULL, startrow=NULL, type=NULL, key=NULL, callopts=list())
{
  url = 'http://services.tropicos.org/Name/Search'
  key <- getkey(key, "tropicosApiKey")
  args <- compact(list(format='json', name=name, nameid=nameid, 
                       commonname=commonname, orderby=orderby, sortorder=sortorder,
                       pagesize=pagesize, startrow=startrow, type=type, apikey=key))
  tt <- GET(url, query=args, callopts)
  stop_for_status(tt)
  out <- content(tt)
  do.call(rbind.fill, lapply(out, data.frame))
}