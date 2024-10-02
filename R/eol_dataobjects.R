#' Given the identifier for a data object, return all metadata about the object
#'
#' @export
#' @param id (character) The EOL data object identifier
#' @param taxonomy (logical) Whether to return any taxonomy details from
#' different taxon hierarchy providers, in an array named `taxonconcepts`
#' @param language (character) provides the results in the specified language.
#' one of ms, de, en, es, fr, gl, it, nl, nb, oc, pt-BR, sv, tl, mk, sr, uk,
#' ar, zh-Hans, zh-Hant, ko
#' @param ... Curl options passed on to [crul::HttpClient]
#' @details It's possible to return JSON or XML with the EOL API. However,
#' this function only returns JSON for now.
#' @return A list, optionally with a data.frame if `taxonomy=TRUE`
#' @examples \dontrun{
#' eol_dataobjects(id = 7561533)
#'
#' # curl options
#' eol_dataobjects(id = 7561533, verbose = TRUE)
#' }
eol_dataobjects <- function(id, taxonomy = TRUE, language = NULL, ...) {
  
  .Defunct("eol", "originr", msg = "This function is defunct since it seems that the data_objects part of the EOL API v1 does not work anymore.")

  # cli <- crul::HttpClient$new(
  #   url = file.path(eol_url("data_objects"), paste0(id, ".json")),
  #   headers = tx_ual,
  #   opts = list(...)
  # )
  # args <- argsnull(tc(list(taxonomy = as_l(taxonomy), language = language)))
  # res <- cli$get(query = args)
  # res$raise_for_status()
  # tt <- res$parse("UTF-8")
  # tmp <- jsonlite::fromJSON(tt)
  # tmp <- nmslwr(tmp)
  # if ("taxonconcepts" %in% names(tmp)) {
  #   tmp$taxonconcepts <- nmslwr(tmp$taxonconcepts)
  #   tmp$taxonconcepts$taxonrank <- tolower(tmp$taxonconcepts$taxonrank)
  # }
  # return(tmp)
}

nmslwr <- function(x) {
  stats::setNames(x, tolower(names(x)))
}
