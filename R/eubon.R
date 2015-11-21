#' EU BON taxonomy
#'
#' @export
#' @param query (character) The scientific name to search for. For example: "Bellis perennis",
#' "Prionus" or "Bolinus brandaris". This is an exact search so wildcard characters
#' are not supported
#' @param providers (character) A list of provider id strings concatenated by comma characters.
#' The default : "pesi,bgbm-cdm-server[col]" will be used if this parameter is not
#' set. A list of all available provider ids can be obtained from the '/capabilities'
#' service end point. Providers can be nested, that is a parent provider can have sub
#' providers. If the id of the parent provider is supplied all subproviders will be
#' queried. The query can also be restriced to one or more subproviders by using the
#' following syntax: parent-id[sub-id-1,sub-id2,...]
#' @param searchMode (character) Specifies the searchMode. Possible search modes are:
#' scientificNameExact, scientificNameLike (begins with), vernacularNameExact, vernacularNameLike
#' (contains), findByIdentifier. If the a provider does not support the chosen searchMode it
#' will be skipped and the status message in the tnrClientStatus will be set to 'unsupported
#' search mode' in this case.
#' @param addSynonymy (logical) Indicates whether the synonymy of the accepted taxon should be
#' included into the response. Turning this option on may cause an increased response time.
#' @param timeout (numeric) The maximum of milliseconds to wait for responses from any of the
#' providers. If the timeout is exceeded the service will jut return the resonses that have
#' been received so far. The default timeout is 0 ms (wait for ever)
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @references \url{http://cybertaxonomy.eu/eubon-utis/doc.html}
#' @examples \dontrun{
#' eubon("Prionus")
#' eubon("Salmo", 'worms')
#' eubon("Salmo", c('pesi', 'worms'))
#' eubon("Salmo", 'worms', 'scientificNameLike')
#' }
eubon <- function(query, providers = 'pesi', searchMode = 'scientificNameExact',
                  addSynonymy = FALSE, timeout = 0, ...) {

  args <- tc(list(query = query, providers = paste0(providers, collapse = ","),
                  searchMode = searchMode, addSynonymy = as_l(addSynonymy),
                  timeout = timeout))
  res <- GET(eubon_base(), query = args, ...)
  stop_for_status(res)
  txt <- content(res, "text")
  jsonlite::fromJSON(txt, FALSE)
}

eubon_base <- function() "http://cybertaxonomy.eu/eubon-utis/search"
