#' EUBON taxonomy search
#'
#' @export
#' @param query (character) The scientific name to search for. For example:
#' "Bellis perennis", "Prionus" or "Bolinus brandaris". This is an exact search
#' so wildcard characters are not supported
#' @param providers (character) A list of provider id strings concatenated by
#' comma characters. The default : "pesi,bgbm-cdm-server[col]" will be used if
#' this parameter is not set. A list of all available provider ids can be
#' obtained from the '/capabilities' service end point. Providers can be
#' nested, that is a parent provider can have sub providers. If the id of the
#' parent provider is supplied all subproviders will be queried. The query
#' can also be restriced to one or more subproviders by using the following
#' syntax: parent-id[sub-id-1,sub-id2,...]
#' @param searchMode (character) Specifies the searchMode. Possible search
#' modes are: scientificNameExact, scientificNameLike (begins with),
#' vernacularNameExact, vernacularNameLike
#' (contains), findByIdentifier. If the a provider does not support the
#' chosen searchMode it will be skipped and the status message in the
#' tnrClientStatus will be set to 'unsupported search mode' in this case.
#' @param addSynonymy (logical) Indicates whether the synonymy of the accepted
#' taxon should be included into the response. Turning this option on may
#' cause an increased response time. Default: \code{FALSE}
#' @param addParentTaxon (logical) Indicates whether the the parent taxon of
#' the accepted taxon should be included into the response. Turning this option
#' on may cause a slightly increased response time. Default: \code{FALSE}
#' @param timeout (numeric) The maximum of milliseconds to wait for responses
#' from any of the providers. If the timeout is exceeded the service will just
#' return the resonses that have been received so far. The default timeout is
#' 0 ms (wait for ever)
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @references \url{http://cybertaxonomy.eu/eu-bon/utis/1.2/doc.html}
#' @details Note that paging is not yet implemented, so you only get the first
#' chunk of up to 50 results for methods that require paging. We will
#' implement paging here when it is available in the EU BON API.
#' @family eubon-methods
#' @examples \dontrun{
#' eubon_search("Prionus")
#' eubon_search("Salmo", 'pesi')
#' eubon_search("Salmo", c('pesi', 'worms'))
#' eubon_search("Salmo", 'worms', 'scientificNameLike')
#' eubon_search("Salmo", 'worms', addSynonymy = TRUE)
#' eubon_search("Salmo", 'worms', addParentTaxon = TRUE)
#' }
eubon <- function(query, providers = 'pesi', searchMode = 'scientificNameExact',
                  addSynonymy = FALSE, addParentTaxon = FALSE, timeout = 0,
                  dedup = NULL, ...) {

  args <- tc(list(query = query, providers = paste0(providers, collapse = ","),
                  searchMode = searchMode, addSynonymy = as_l(addSynonymy),
                  addParentTaxon = as_l(addParentTaxon), timeout = timeout))
  res <- httr::GET(file.path(eubon_base(), "search"), query = args, ...)
  eubon_error(res)
  tmp <- jsonlite::fromJSON(con_utf8(res), TRUE, flatten = TRUE)
  tmp$query$response[[1]]
}

#' @export
#' @rdname eubon
eubon_search <- eubon

# helpers
eubon_base <- function() "http://cybertaxonomy.eu/eu-bon/utis/1.2"

eubon_error <- function(x) {
  if (grepl("json", x$headers$`content-type`)) {
    cs <- jsonlite::fromJSON(con_utf8(x), FALSE)$query[[1]]$clientStatus[[1]]
    if (x$status_code > 201 || cs$statusMessage != "ok") {
      stop(cs$statusMessage, call. = FALSE)
    }
  } else if (grepl("html", x$headers$`content-type`)) {
    if (x$status_code > 201) {
      mssg <- xml2::xml_text(xml2::xml_find_first(xml2::read_html(con_utf8(x)),
                                                 "//title"))
      stop(mssg, call. = FALSE)
    }
  } else {
    stop(httr::http_status(x)$message, call. = FALSE)
  }
}
