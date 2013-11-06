#' Search Catalogue of Life for taxonomic IDs
#' 
#' @import RCurl XML plyr
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
#' @details You must provide one of name or id. The other parameters (format 
#' 		and start) are optional.
#' @return A list of data.frame's.
#' @examples \dontrun{
#' # A basic example
#' col_search(name="Apis")
#' 
#' # Many names
#' col_search(name=c("Apis","Puma concolor"))
#' 
#' # An example where there is no data
#' col_search(id=11935941)
#' }
#' @export
col_search <- function(name=NULL, id=NULL, start=NULL, checklist=NULL)
{
  url <- "http://www.catalogueoflife.org/col/webservice"
  func <- function(x, y) {
    if(is.null(checklist)){NULL} else {
      cc <- match.arg(checklist, choices = c(2012, 2011, 2010, 2009, 2008, 2007))
      if (cc %in% c(2012, 2011, 2010)){
        url <- gsub("col", paste("annual-checklist/", cc, sep = ""), url)
      } else {
        url <- "http://webservice.catalogueoflife.org/annual-checklist/year/search.php"
        url <- gsub("year", cc, url)
      }
    }
    args <- compact(list(name = x, id = y, start = start))
    out <- getForm(url, .params = args)
    tt <- xmlParse(out)
    toget <- c('id','name','rank','name_status')
    nodes <- getNodeSet(tt, "//result", fun=xmlToList)
    ldply(nodes, parsecoldata)
  }
  safe_func <- plyr::failwith(NULL, func)
  if(is.null(id)){ 
    temp <- lapply(name, safe_func, y = NULL)
    names(temp) <- name
  } else { 
    temp <- lapply(id, safe_func, x = NULL) 
    names(temp) <- id
  }
  return(temp)
}

parsecoldata <- function(x){
  vals <- x[c('id','name','rank','name_status','source_database')]
  vals[sapply(vals, is.null)] <- NA
  names(vals) <- c('id','name','rank','name_status','source_database')
  bb <- data.frame(vals, stringsAsFactors=FALSE)
  names(bb)[4:5] <- c('status','source')
  acc <- x$accepted_name
  if(is.null(acc)){
    accdf <- data.frame(acc_id=NA, acc_name=NA, acc_rank=NA, acc_status=NA, acc_source=NA)
  } else
  {
    accdf <- data.frame(acc[c('id','name','rank','name_status','source_database')], stringsAsFactors=FALSE)
    names(accdf) <- c('acc_id','acc_name','acc_rank','acc_status','acc_source')
  }
  cbind(bb, accdf)
}