#' @title TNRS sources
#'
#' @description Get sources for the Phylotastic Taxonomic Name 
#' Resolution Service
#'
#' @param source The source to get information on, one of "iPlant_TNRS",
#' "NCBI", or "MSW3".
#' @param ... Curl options to pass in \code{\link[crul]{verb-GET}}
#' @return Sources for the TNRS API in a vector or list
#' @export
#' @examples \dontrun{
#' # All
#' tnrs_sources()
#'
#' # A specific source
#' tnrs_sources(source="NCBI")
#' tnrs_sources(source="MSW3")
#' tnrs_sources(source="iPlant_TNRS")
#' }
tnrs_sources <- function(source = NULL, ...) {
  cli <- crul::HttpClient$new(tnrs_url, headers = tx_ual, opts = list(...))
  if (!is.null(source)) {
    tt <- cli$get(file.path("sources", source))
    tt$raise_for_status()
    res <- tt$parse("UTF-8")
    tmp <- nmslwr(jsonlite::fromJSON(res))
    tmp$details <- nmslwr(tmp$details)
    tmp
  } else {
    tt <- cli$get("sources/list")
    tt$raise_for_status()
    res <- tt$parse("UTF-8")
    jsonlite::fromJSON(res)$sources
  }
}

tnrs_url <- "http://taxosaurus.org"
