#' Get data sources for the Global Names Resolver.
#' 
#' Retrieve data sources used in Global Names Index, see 
#'    \url{http://gni.globalnames.org/} for information. 
#' 
#' @import jsonlite plyr
#' @param todf logical; Should a data.frame be returned?
#' 
#' @author Scott Chamberlain {myrmecocystus@@gmail.com}
#' @return json or a data.frame
#' @seealso \code{\link[taxize]{gnr_resolve}}
#' @keywords resolve names taxonomy
#' @export
#' @examples \dontrun{
#' # all data sources
#' gnr_datasources() 
#' 
#' # give me the id for EOL
#' out <- gnr_datasources()
#' out[out$title == "EOL", "id"]
#' 
#' # Fuzzy search for sources with the word zoo
#' out <- gnr_datasources()
#' out[agrep("zoo", out$title, ignore.case = TRUE), ]
#' 
#' # Output as a list
#' gnr_datasources(FALSE)
#' }
gnr_datasources <- function(todf = TRUE) 
{
  url <- "http://resolver.globalnames.org/data_sources.json"
	if (todf == FALSE){	
		out <- jsonlite::fromJSON(url, FALSE)
	} else {
    out <- ldply(jsonlite::fromJSON(url, FALSE), function(x) data.frame(x["id"], x["title"], stringsAsFactors = FALSE))
	}
  return(out)
}
