#' Search Tropicos by scientific name, common name, or Tropicos ID.
#'
#' @export
#'
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
#' @param ... Further args passed on to \code{\link[httr]{GET}}
#' @return List or dataframe.
#' @references \url{http://services.tropicos.org/help?method=SearchNameXml}
#' @examples \dontrun{
#' tp_search(name = 'Poa annua')
#' }
tp_search <- function(name=NULL, commonname=NULL, nameid=NULL, orderby=NULL,
  sortorder=NULL, pagesize=NULL, startrow=NULL, type=NULL, key=NULL, ...) {

  url = 'http://services.tropicos.org/Name/Search'
  key <- getkey(key, "tropicosApiKey")
  args <- taxize_compact(list(format='json', name=name, nameid=nameid,
                       commonname=commonname, orderby=orderby, sortorder=sortorder,
                       pagesize=pagesize, startrow=startrow, type=type, apikey=key))
  tt <- GET(url, query = args, ...)
  warn_for_status(tt)
  if (tt$status_code > 202) {
    NA
  } else {
    out <- content(tt)
    tmp <- do.call(rbind.fill, lapply(out, data.frame, stringsAsFactors = FALSE))
    setNames(tmp, tolower(names(tmp)))
  }
}
