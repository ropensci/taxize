#' Resolve names using Global Names Resolver.
#' 
#' Uses the Global Names Index, see \url{http://gni.globalnames.org/} for information. 
#' 
#' @import stringr RJSONIO RCurl plyr
#' @param names Quoted taxonomic names to be resolved in a vector.
#' @param data_source_ids Supply data source IDs to specify what data source
#' 		is searched. See example below.
#' @param resolve_once Default is false. Find the first available match instead of 
#'    matches across all data sources with all possible renderings of a name. 
#'    When 'true', response is rapid but incomplete (logical)
#' @param with_context Reduce the likelihood of matches to taxonomic homonyms. 
#'    When 'true' a common taxonomic context is calculated for all supplied names 
#'    from matches in data sources that have classification tree paths. Names out 
#'    of determined context are penalized during score calculation.
#' @param stripauthority If TRUE, gives back names with taxonomic authorities. If FALSE, 
#'    strips author names.
#' @param highestscore Return those names with the highest score for each searched name.
#' @author Scott Chamberlain {myrmecocystus@@gmail.com}
#' @return A data.frame.
#' @keywords resolve names taxonomy
#' @examples \dontrun{
#' gnr_resolve(names = c("Helianthus annuus", "Homo sapiens"), returndf = TRUE)
#' gnr_resolve(names = c("Asteraceae", "Plantae"), returndf = TRUE)
#' 
#' # Using data source 12 (Encyclopedia of Life)
#' gnr_resolve(names = c("Helianthos annuus", "Homo sapians"), returndf = TRUE)
#' }
#' @export
gnr_resolve <- function(names, data_source_ids = NULL, resolve_once = FALSE, 
    with_context = FALSE, stripauthority = FALSE, highestscore = TRUE)
{
  url = "http://resolver.globalnames.org/name_resolvers"
	url <- paste(url, ".json", "?", sep="")
	names2 <- paste("names=", paste(str_replace_all(names, " ", "+"), collapse = "|", sep=""), sep="")
	if(!is.null(data_source_ids)){
		data_source_ids2 <- paste("&data_source_ids=", data_source_ids, sep="")} else
			{ data_source_ids2 <- data_source_ids }
	if(resolve_once){ resolve_once2 <- "&resolve_once=true" } else { resolve_once2 <- NULL }
	if(with_context){ with_context2 <- "&with_context=true" } else { with_context2 <- NULL }
	query <- paste(compact(list(url, names2, data_source_ids2, resolve_once2, with_context2)), collapse="")
  
  data <- fromJSON(query)$data
  data_ <- llply(data, function(y) list(y[["supplied_name_string"]], 
                llply(y$results, function(x) data.frame(x[c("name_string", "data_source_title", "score", "canonical_form")]))))
  data_2 <- ldply(data_, function(x) data.frame(x[[1]], ldply(x[[2]])))
  names(data_2)[c(1,2,5)] <- c("submitted_name","matched_name","matched_name2")
  out <- data_2[order(data_2$submitted_name),]
  
  if(stripauthority){
    out[,!names(out) %in% "matched_name"]
  } else
  {
    out[,!names(out) %in% "matched_name2"]
  }        
}