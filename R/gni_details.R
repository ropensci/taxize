#' @title Search for taxonomic name details using the Global Names Index
#'
#' @description Uses the Global Names Index, see http://gni.globalnames.org/
#'
#' @export
#' @param id Name id. Required.
#' @param all_records If all_records is 1, GNI returns all records from all
#' 		repositories for the name string (takes 0, or 1 \[default\]).
#' @param ... Curl options passed on to [crul::verb-GET]
#' @author Scott Chamberlain
#' @return Data.frame of results.
#' @seealso [gnr_datasources()], [gna_search()].
#' @keywords globalnamesindex names taxonomy
#' @examples \dontrun{
#' gni_details(id = 17802847)
#'
#' # pass on curl options
#' gni_details(id = 17802847, verbose = TRUE)
#' }
gni_details <- function(id, all_records = 1, ...) {
  .Defunct(msg = "This function is defunct - See ?`taxize-defunct`")
  
#   calls <- names(sapply(match.call(), deparse))[-1]
#   calls_vec <- "url" %in% calls
#   if (any(calls_vec)) stop("The parameter url has been removed", call. = FALSE)
# 
# 	url2 <- paste0(gni_base(), "name_strings/", id, ".json")
# 	query <- tc(list(all_records = all_records))
# 	cli <- crul::HttpClient$new(url2, headers = tx_ual, opts = list(...))
# 	tt <- cli$get(query = argsnull(query))
# 	tt$raise_for_status()
#   out <- jsonlite::fromJSON(tt$parse("UTF-8"), FALSE)
# 	outdf <-
# 		dt2df(lapply(out$data, function(x)
# 			data.frame(t(c(checknull(x$records[[1]]$created_at),
# 				checknull(x$records[[1]]$updated_at),
# 				checknull(x$records[[1]]$global_id),
# 				checknull(x$records[[1]]$url),
# 				checknull(x$records[[1]]$kingdom_id),
# 				checknull(x$records[[1]]$original_name_string),
# 				checknull(x$records[[1]]$id),
# 				checknull(x$records[[1]]$name_rank_id),
# 				checknull(x$records[[1]]$name_index_id),
# 				checknull(x$records[[1]]$record_hash),
# 				checknull(x$records[[1]]$local_id),
# 				checknull(x$records[[1]]$nomenclatural_code_id) )))), idcol = FALSE)
# 	stats::setNames(outdf, c(
# 	  "created_at","updated_at","global_id","url","kingdom_id",
# 	  "original_name_string","id","name_rank_id","name_index_id","record_hash",
# 	  "local_id","nomenclatural_code_id"
# 	))
}
