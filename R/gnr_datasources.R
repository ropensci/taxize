#' Get data sources for the Global Names Resolver.
#' 
#' Uses the Global Names Index, see \url{http://gni.globalnames.org/} 
#' 		for information. 
#' 
#' @import RJSONIO plyr
#' @param todf Parse id and name of provider to data.frame? logical (default = FALSE).
#' @param url Base url for the API; leave as is.
#' @author Scott Chamberlain {myrmecocystus@@gmail.com}
#' @return json or xml output, your choice
#' @keywords resolve names taxonomy
#' @examples \dontrun{
#' # all data sources
#' gnr_datasources() 
#' 
#' # just id's and names of sources
#' gnr_datasources(todf=T)
#' 
#' # give me the id for EOL
#' out <- gnr_datasources(todf=T)
#' out[out$title == "EOL", "id"]
#' 
#' # Fuzzy search for sources with the word zoo
#' out <- gnr_datasources(todf=T)
#' out[agrep("zoo", out$title, ignore.case=T), ]
#' }
#' @export
gnr_datasources <- function(todf = FALSE,
	url = "http://resolver.globalnames.org/data_sources.json") 
{
	if(todf == FALSE){	
		fromJSON(url)
	} else
		{
			ldply(fromJSON(url), function(x) data.frame(x["id"], x["title"]))
		}
}