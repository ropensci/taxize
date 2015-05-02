#' Search Catalogue of Life for taxonomic IDs
#'
#' @import RCurl XML plyr
#' @export
#' @param name The string to search for. Only exact matches found the name given
#'   	will be returned, unless one or wildcards are included in the search
#'   	string. An * (asterisk) character denotes a wildcard; a % (percentage)
#'    character may also be used. The name must be at least 3 characters long,
#'    not counting wildcard characters.
#' @param id The record ID of the specific record to return (only for scientific
#' 		names of species or infraspecific taxa)
#' @param start The first record to return. If omitted, the results are returned
#' 		from the first record (start=0). This is useful if the total number of
#' 		results is larger than the maximum number of results returned by a single
#' 		Web service query (currently the maximum number of results returned by a
#' 		single query is 500 for terse queries and 50 for full queries).
#' @param checklist The year of the checklist to query, if you want a specific
#' 		year's checklist instead of the lastest as default (numeric).
#' @param response (character) one of "terse" or "full"
#' @details You must provide one of name or id. The other parameters (format
#' 		and start) are optional.
#' @references \url{http://webservice.catalogueoflife.org/}
#' @return A list of data.frame's.
#' @examples \dontrun{
#' # A basic example
#' col_search(name="Apis")
#' col_search(name="Agapostemon")
#' col_search(name="Poa")
#'
#' # Many names
#' col_search(name=c("Apis","Puma concolor"))
#'
#' # An example where there is no data
#' col_search(id=11935941)
#' }

col_search <- function(name=NULL, id=NULL, start=NULL, checklist=NULL, response="terse", ...) {
  response <- match.arg(response, c("terse", "full"))
  func <- function(x, y) {
    if (is.null(checklist)) {
      url <- col_base()
    } else {
      cc <- match.arg(checklist, choices = c(2012, 2011, 2010, 2009, 2008, 2007))
      if (cc %in% c(2012, 2011, 2010)) {
        url <- gsub("col", paste("annual-checklist/", cc, sep = ""), col_base())
      } else {
        url <- "http://webservice.catalogueoflife.org/annual-checklist/year/search.php"
        url <- gsub("year", cc, url)
      }
    }
    args <- compact(list(name = x, id = y, start = start, response = response))
    temp <- GET(url, query = args, ...)
    stop_for_status(temp)
    tt <- content(temp)
    switch(response,
           terse = parse_terse(tt),
           full = parse_full(tt))
  }
  safe_func <- plyr::failwith(NULL, func)
  if (is.null(id)) {
    setNames(lapply(name, safe_func, y = NULL, ...), name)
  } else {
    setNames(lapply(id, safe_func, x = NULL, ...), id)
  }
}

col_base <- function() "http://www.catalogueoflife.org/col/webservice"

parse_terse <- function(x) {
  nodes <- getNodeSet(x, "//result", fun = xmlToList)
  ldply(nodes, parsecoldata)
}

parse_full <- function(x) {
  tmp <- getNodeSet(x, "//result")
  taxize_ldfast(lapply(tmp, function(z) {
    switch(xpathSApply(z, "name_status", xmlValue),
           `accepted name` = {
             h_vals <- xpathSApply(z, "classification//name", xmlValue)
             h_nms <- xpathSApply(z, "classification//rank", xmlValue)
             h <- setNames(rbind.data.frame(h_vals), h_nms)
           },
           synonym = {
             h_vals <- xpathSApply(z, "accepted_name//classification//name", xmlValue)
             h_nms <- xpathSApply(z, "accepted_name//classification//rank", xmlValue)
             h <- setNames(rbind.data.frame(h_vals), h_nms)
           })
    target <- setNames(rbind.data.frame(
      c(xpathSApply(z, "name", xmlValue),
        xpathSApply(z, "rank", xmlValue),
        xpathSApply(z, "id", xmlValue),
        xpathSApply(z, "name_status", xmlValue))),
      c("name", "rank", "id", "name_status"))
    tempdf <- cbind(target, h)
    tempdf[] <- lapply(tempdf, as.character)
    tempdf
  }))
}

parsecoldata <- function(x){
  vals <- x[c('id', 'name', 'rank', 'name_status', 'source_database')]
  vals[sapply(vals, is.null)] <- NA
  names(vals) <- c('id', 'name', 'rank', 'name_status', 'source_database')
  bb <- data.frame(vals, stringsAsFactors = FALSE)
  names(bb)[4:5] <- c('status', 'source')
  acc <- x$accepted_name
  if (is.null(acc)) {
    accdf <- data.frame(acc_id=NA, acc_name=NA, acc_rank=NA, acc_status=NA, acc_source=NA, stringsAsFactors = FALSE)
  } else {
    accdf <- data.frame(acc[c('id','name','rank','name_status','source_database')], stringsAsFactors=FALSE)
    names(accdf) <- c('acc_id','acc_name','acc_rank','acc_status','acc_source')
  }
  cbind(bb, accdf)
}
