#' @title TNRS sources
#'
#' @description Get sources for the Phylotastic Taxonomic Name Resolution Service
#'
#' @param source The source to get information on, one of "iPlant_TNRS",
#' "NCBI", or "MSW3".
#' @param ... Curl options to pass in \code{\link[httr]{GET}}
#' @return Sources for the TNRS API in a vector or list
#' @export
#' @examples \dontrun{
#' # All
#' tnrs_sources()
#'
#' # A specific source
#' tnrs_sources(source="NCBI")
#' }
tnrs_sources <- function(source = NULL, ...) {
  url = "http://taxosaurus.org/sources"
  if (!is.null(source)) {
    url2 <- paste0(url, "/", source)
    tt <- GET(url2, ...)
    stop_for_status(tt)
    res <- content(tt, as = "text")
    jsonlite::fromJSON(res)
  } else {
    url2 <- paste0(url, "/list")
    tt <- GET(url2, ...)
    stop_for_status(tt)
    res <- content(tt, as = "text")
    jsonlite::fromJSON(res)$sources
  }
}
