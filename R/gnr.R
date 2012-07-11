#' Resolve names using Global Names Resolver.
#' 
#' Uses the Global Names Index, see \link{http://gni.globalnames.org/} for information. 
#' 
#' @import httr XML stringr RJSONIO RCurl
#' @param names Quoted taxonomic names to be resolved in a vector.
#' @param data_source_ids Supply data source IDs to specify what data source
#' 		is searched. See example below.
#' @param output Output format for the results, one of json (default) or xml.
#' @param url Base url for the API; leave as is.
#' @author Scott Chamberlain {myrmecocystus@@gmail.com}
#' @return json or xml output, your choice
#' @seealso \code{\link{tnrastic}} and \code{\link{tnrsmatch}}.
#' @keywords resolve names taxonomy
#' @examples \dontrun{
#' gnr(names = c("Helianthus annuus", "Homo sapiens"))
#' gnr(names = c("Helianthus annuus", "Homo sapiens"), data_source_ids="12")
#' gnr(names = c("Helianthus annuus", "Homo sapiens"), output="xml")
#' }
#' @export
gnr <- function(names, data_source_ids = NULL, output = "json",
		url = "http://resolver.globalnames.org/name_resolvers") 
{
	if(output == "json"){url <- paste(url, ".json", "?", sep="")} else
		if(output == "xml"){url <- paste(url, ".xml", "?", sep="")} else
			stop(paste(output, " is not a supported output format", sep=""))
	names2 <- paste("names=", paste(str_replace_all(names, " ", "+"), collapse = "|", sep=""), sep="")
	if(!is.null(data_source_ids)){
		data_source_ids2 <- paste("&data_source_ids=", data_source_ids, sep="")} else
			{ data_source_ids2 <- data_source_ids }
	query <- paste(compact(list(url, names2, data_source_ids2)), collapse="")
	if(output == "json"){ fromJSON(query) } else
		if(output == "xml"){ xmlParse(getURL(query)) } else
			stop(paste(output, " is not a supported output format", sep=""))
}