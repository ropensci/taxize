#' Search Tropicos by scientific name, common name, or Tropicos ID.
#'
#' @export
#' @param name Your search string. For instance "poa annua". See Details.
#' @param commonname Your search string. For instance "annual blue grass"
#' @param nameid Your search string. For instance "25509881"
#' @param orderby Your search string. For instance "1"
#' @param sortorder Your search string. For instance "ascending"
#' @param pagesize Your search string. For instance "100"
#' @param startrow Your search string. For instance "1"
#' @param type Type of search, "wildcard" (default) will add a wildcard to the end
#'    of your search string. "exact" will use your search string exactly.
#' @param key Your Tropicos API key; See \code{\link{taxize-authentication}} 
#' for help on authentication
#' @param ... Further args passed on to \code{\link[crul]{HttpClient}}
#' @return List or dataframe.
#' @references \url{http://services.tropicos.org/help?method=SearchNameXml}
#' @details More details on the \code{name} parameter: Tropicos will fail
#' if you include a period (\code{.}) in your name string, e.g., \code{var.},
#' so we replace periods before the request is made to the Tropicos web service.
#' In addition, Tropicos for some reason doesn't want to see sub-specific rank
#' names like \code{var}/\code{subsp}, so remove those from your query.
#' @examples \dontrun{
#' tp_search(name = 'Poa annua')
#' tp_search(name = 'Poa annua subsp. annua')
#' tp_search(name = 'Poa annua var. annua')
#' tp_search(name = 'Poa annua var annua')
#' tp_search(name = 'Poa annua annua')
#' }
tp_search <- function(name=NULL, commonname=NULL, nameid=NULL, orderby=NULL,
  sortorder=NULL, pagesize=NULL, startrow=NULL, type=NULL, key=NULL, ...) {

  url = 'http://services.tropicos.org/Name/Search'
  key <- getkey(key, "TROPICOS_KEY")
  if (!is.null(name)) {
    if (grepl(paste(sprintf("\\s%s\\.?\\s", subsp_ranks), collapse = "|"), name)) {
      warning("Tropicos doesn't like sub-specific ranks - remove them in your query", call. = FALSE)
    }
    if (grepl("\\.", name)) {
      warning("'.' detected, being URL encoded prior to data request", call. = FALSE)
    }
    name <- gsub("\\.", "%2E", name)
  }
  args <- tc(list(format='json', name=name, nameid=nameid,
                  commonname=commonname, orderby=orderby, sortorder=sortorder,
                  pagesize=pagesize, startrow=startrow, type=type, apikey=key))
  tt <- tp_GET(url, args, raise = FALSE, ...)
  if (tt$status_code > 202) {
    warning(tt$status_code, " - problem with request")
    NA
  } else {
    out <- jsonlite::fromJSON(tt$parse("UTF-8"), FALSE)
    tmp <- do.call(rbind.fill, lapply(out, data.frame, stringsAsFactors = FALSE))
    setNames(tmp, tolower(names(tmp)))
  }
}

subsp_ranks <- c('sp', 'ssp', 'subsp', 'subspecies', 'var', 'varietas', 'fo', 'f', 'forma')
