#' Get sources for the Phylotastic Taxonomic Name Resolution Service.
#'
#' THIS FUNCTION IS DEFUNCT.
#'
#' @param source The source to get information on, one of "iPlant_TNRS",
#' "NCBI", or "MSW3".
#' @return Sources for the TNRS API.
#' @rdname tnrs_sources-defunct
#' @export
tnrs_sources <- function(source = NULL) {
  .Defunct(msg = "This function is defunct. See ?taxize-defunct")

	url = "http://taxosaurus.org/sources"
	if (!is.null(source)) {
		url2 <- paste0(url, "/", source)
    tt <- GET(url2)
    stop_for_status(tt)
    res <- content(tt, as = "text")
		jsonlite::fromJSON(res)
	} else {
		url2 <- paste0(url, "/list")
		tt <- GET(url2)
		stop_for_status(tt)
		res <- content(tt, as = "text")
		jsonlite::fromJSON(res)$sources
	}
}
