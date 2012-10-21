#' Resolve names using Global Names Resolver.
#' 
#' Uses the Global Names Index, see \url{http://gni.globalnames.org/} for information. 
#' 
#' @import stringr RJSONIO RCurl plyr
#' @param names Quoted taxonomic names to be resolved in a vector.
#' @param data_source_ids Supply data source IDs to specify what data source
#' 		is searched. See example below.
#' @param returndf Return data.frame or list (logical; default data.frame).
#' @param url Base url for the API; leave as is.
#' @author Scott Chamberlain {myrmecocystus@@gmail.com}
#' @return json or xml output, your choice
#' @seealso \code{\link{iplant_tnrsmatch}}.
#' @keywords resolve names taxonomy
#' @examples \dontrun{
#' gnr_resolve(names = c("Helianthus annuus", "Homo sapiens"), returndf = TRUE)
#' gnr_resolve(names = c("Asteraceae", "Plantae"), returndf = TRUE)
#' 
#' # Using data source 12 (Encyclopedia of Life)
#' gnr_resolve(names = c("Helianthus annuus", "Homo sapiens"), data_source_ids="12", returndf = TRUE)
#' }
#' @export
gnr_resolve <- function(names, data_source_ids = NULL, returndf = FALSE,
		url = "http://resolver.globalnames.org/name_resolvers") 
{
	url <- paste(url, ".json", "?", sep="")
	names2 <- paste("names=", paste(str_replace_all(names, " ", "+"), collapse = "|", sep=""), sep="")
	if(!is.null(data_source_ids)){
		data_source_ids2 <- paste("&data_source_ids=", data_source_ids, sep="")} else
			{ data_source_ids2 <- data_source_ids }
	query <- paste(compact(list(url, names2, data_source_ids2)), collapse="")
	if(returndf == FALSE){
		fromJSON(query) 
	} else
		{
			data <- fromJSON(query)$data
			data_ <- llply(data, function(y) list(y[["supplied_name_string"]], 
					llply(y$results, function(x) data.frame(x["name_string"], 
								x["data_source_id"], x["score"]))))
			data_2 <- ldply(data_, function(x) data.frame(x[[1]], ldply(x[[2]])))
			sources <- gnr_datasources(todf=T)
			data_2_ <- merge(data_2, sources, by.x="data_source_id", by.y="id")
			names(data_2_)[2] <- c("submitted_name")
			data_2_[order(data_2_$submitted_name),]
		}
}