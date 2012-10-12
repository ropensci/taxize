#' Search for taxonomic name details using the Global Names Index.
#' 
#' Uses the Global Names Index, see \link{http://gni.globalnames.org/} for information. 
#' 
#' @import stringr RJSONIO RCurl plyr
#' @param id Name id.
#' @param all_records  If all_records is 1, GNI returns all records from all 
#' 		repositories for the name string (takes 0, or 1, default is 1).
#' @param url Base url for the API; leave as is.
#' @author Scott Chamberlain {myrmecocystus@@gmail.com}
#' @return Data.frame of results.
#' @seealso \code{\link{gnr_datasources}}, \code{\link{gni_search}}.
#' @keywords globalnamesindex names taxonomy
#' @examples \dontrun{
#' gni_details(id = 17802847)
#' ldply(list(1265133, 17802847), gni_details)
#' }
#' @export
gni_details <- function(id = NULL, all_records = NULL, 
				url = "http://gni.globalnames.org/name_strings/") 
{
	url2 <- paste(url, id, ".json", sep="")
	query <- compact(list(all_records = all_records))
	out <- parsed_content( GET(url2, query = query) )
	checknull <- function(x) {if(is.null(x)){"none"} else{x}}
	outdf <- 
		ldply(out$data, function(x) data.frame(t(c(checknull(x$records[[1]]$created_at),
					checknull(x$records[[1]]$updated_at), checknull(x$records[[1]]$global_id),
					checknull(x$records[[1]]$url), checknull(x$records[[1]]$kingdom_id),
					checknull(x$records[[1]]$original_name_string), checknull(x$records[[1]]$id),
					checknull(x$records[[1]]$name_rank_id), checknull(x$records[[1]]$name_index_id),
					checknull(x$records[[1]]$record_hash), checknull(x$records[[1]]$local_id),
					checknull(x$records[[1]]$nomenclatural_code_id) ))))
	names(outdf) <- c(
		"created_at","updated_at","global_id","url","kingdom_id","original_name_string",
		"id","name_rank_id","name_index_id","record_hash","local_id","nomenclatural_code_id"
		)
	outdf
}