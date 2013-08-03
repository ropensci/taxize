#' Get sources for the Phylotastic Taxonomic Name Resolution Service.
#'
#' @import RJSONIO plyr
#' @param source The source to get information on, one of "iPlant_TNRS", 
#' "NCBI", or "MSW3".
#' @return Sources for the TNRS API.
#' @export
#' @examples \dontrun{
#' # All
#' tnrs_sources()
#' 
#' # A specific source
#' tnrs_sources(source="NCBI")
#' }
tnrs_sources <- function(source = NULL)
{
	url = "http://taxosaurus.org/sources"
	if(!is.null(source)) {
		url2 <- paste0(url, "/", source)
		fromJSON(url2)
	} else 
	{
		url2 <- paste0(url, "/list")
		fromJSON(url2)$sources
	}
}