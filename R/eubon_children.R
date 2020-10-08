#' EUBON children
#'
#' @export
#' @param id (character) identifier for the taxon. (LSID, DOI, URI, or any
#' other identifier used by the checklist provider)
#' @param providers (character) A list of provider id strings concatenated by
#' comma characters. The default : "pesi,bgbm-cdm-server\[col\] will
#' be used if this parameter is not set. A list of all available provider ids
#' can be obtained from the '/capabilities' service end point. Providers can be
#' nested, that is a parent provider can have sub providers. If the id of the
#' parent provider is supplied all subproviders will be queried. The query
#' can also be restricted to one or more subproviders by using the following
#' syntax: parent-id\[sub-id-1,sub-id2,...\]
#' @param timeout (numeric) The maximum of milliseconds to wait for responses
#' from any of the providers. If the timeout is exceeded the service will just
#' return the responses that have been received so far. The default timeout is
#' 0 ms (wait for ever)
#' @param ... Curl options passed on to [crul::verb-GET]
#' @references https://cybertaxonomy.eu/eu-bon/utis/1.3/doc.html
#' @return a data.frame or an empty list if no results found
#' @note There is no pagination in this method, so you may or may not be
#' getting all the results for a search. Sorry, out of our control
#' @family eubon-methods
#' @examples \dontrun{
#' x <- eubon_children(id = "urn:lsid:marinespecies.org:taxname:126141",
#'   providers = 'worms')
#' head(x)
#' }
eubon_children <- function(id, providers = NULL, timeout = 0, ...) {
  args <- tc(list(providers = paste0(providers, collapse = ","),
    timeout = timeout))
  url <- file.path(eubon_base(), "classification", id, "children")
  cli <- crul::HttpClient$new(url, headers = tx_ual, opts = list(...))
  res <- cli$get(query = args)
  eubon_error(res)
  tmp <- jsonlite::fromJSON(res$parse("UTF-8"), TRUE, flatten = TRUE)
  tmp$query$response[[1]]
}
