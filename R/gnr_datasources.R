#' Get data sources for the Global Names Resolver.
#' 
#' Uses the Global Names Index, see \link{http://gni.globalnames.org/} 
#' 		for information. 
#' 
#' @import RJSONIO
#' @param todf Parse id and name of provider to data.frame? logical (default = FALSE).
#' @param url Base url for the API; leave as is.
#' @author Scott Chamberlain {myrmecocystus@@gmail.com}
#' @return json or xml output, your choice
#' @seealso \code{\link{tnrastic}} and \code{\link{tnrsmatch}}.
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
#' out[out$title %in% "EOL", "id"]
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
			ldply(mm, function(x) data.frame(x["id"], x["title"]))
		}
}