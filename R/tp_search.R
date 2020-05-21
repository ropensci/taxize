#' Search Tropicos by scientific name, common name, or Tropicos ID.
#'
#' @export
#' @param sci A scientific name, e.g., "poa annua". See Details.
#' @param com A common name, e.g., "annual blue grass"
#' @param nameid Your search string. e.g., "25509881"
#' @param orderby Your search string. e.g., "1"
#' @param sortorder Your search string. e.g., "ascending"
#' @param pagesize Your search string. e.g., "100"
#' @param startrow Your search string. e.g., "1"
#' @param type Type of search, "wildcard" (default) will add a wildcard to
#' the end of your search string. "exact" will use your search string exactly.
#' @param key Your Tropicos API key; See [taxize-authentication] 
#' for help on authentication
#' @param name Deprecated, see `sci`
#' @param commonname Deprecated, see `com`
#' @param ... Further args passed on to [crul::HttpClient]
#' @return List or dataframe.
#' @references <http://services.tropicos.org/help?method=SearchNameXml>
#' @details More details on the `name` parameter: Tropicos will fail
#' if you include a period (`.`) in your name string, e.g., `var.`,
#' so we replace periods before the request is made to the Tropicos web service.
#' In addition, Tropicos for some reason doesn't want to see sub-specific rank
#' names like `var`/`subsp`, so remove those from your query.
#' @examples \dontrun{
#' tp_search(sci = 'Poa annua')
#' tp_search(sci = 'Poa annua subsp. annua')
#' tp_search(sci = 'Poa annua var. annua')
#' tp_search(sci = 'Poa annua var annua')
#' tp_search(sci = 'Poa annua annua')
#' }
tp_search <- function(sci=NULL, com=NULL, nameid=NULL, orderby=NULL,
  sortorder=NULL, pagesize=NULL, startrow=NULL, type=NULL, key=NULL, name=NULL,
  commonname=NULL, ...) {

  pchk(name, "sci")
  pchk(commonname, "com")
  if (!is.null(name)) sci <- name
  if (!is.null(commonname)) com <- commonname
  url = 'http://services.tropicos.org/Name/Search'
  key <- getkey(key, "TROPICOS_KEY")
  if (!is.null(sci)) {
    if (grepl(paste(sprintf("\\s%s\\.?\\s", subsp_ranks), collapse = "|"), sci)) {
      warning("Tropicos doesn't like sub-specific ranks - remove them in your query",
        call. = FALSE)
    }
    if (grepl("\\.", sci)) {
      warning("'.' detected, being URL encoded prior to data request",
        call. = FALSE)
    }
    sci <- gsub("\\.", "%2E", sci)
  }
  args <- tc(list(format='json', name=sci, nameid=nameid,
                  commonname=com, orderby=orderby, sortorder=sortorder,
                  pagesize=pagesize, startrow=startrow, type=type, apikey=key))
  tt <- tp_GET(url, args, raise = FALSE, ...)
  out <- jsonlite::fromJSON(tt, FALSE)
  tmp <- dt2df(lapply(out, data.frame, stringsAsFactors = FALSE), idcol = FALSE)
  stats::setNames(tmp, tolower(names(tmp)))
}

subsp_ranks <- c('sp', 'ssp', 'subsp', 'subspecies', 'var', 'varietas', 'fo', 'f', 'forma')
