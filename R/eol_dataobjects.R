#' Given the identifier for a data object, return all metadata about the object
#'
#' @export
#' @param id (character) The EOL data object identifier
#' @param taxonomy (logical) Whether to return any taxonomy details from different
#' taxon hierarchy providers, in an array named \code{taxonconcepts}
#' @param usekey (logical) use your API key or not (\code{TRUE} or \code{FALSE})
#' @param key (character) Your EOL API key; can load from .Rprofile if not passed as a parameter
#' @param verbose (logical); If TRUE the actual taxon queried is printed on the
#'    console.
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @details It's possible to return JSON or XML with the EOL API. However,
#' 		this function only returns JSON for now.
#' @return A list, optionally with a data.frame if \code{taxonomy=TRUE}
#' @examples \dontrun{
#' eol_dataobjects(id = "d72801627bf4adf1a38d9c5f10cc767f")
#' eol_dataobjects(id = "21929584")
#'
#' # curl options
#' library("httr")
#' eol_dataobjects(id = "21929584", config = verbose())
#' }
eol_dataobjects <- function(id, taxonomy = TRUE, usekey = TRUE, key = NULL, verbose = TRUE, ...) {
  if (usekey) key <- getkey(key, "eolApiKey")
  tt <- GET(file.path(eol_url("data_objects"), paste0(id, ".json")),
            query = argsnull(tc(list(key = key, taxonomy = as_l(taxonomy)))), ...)
  stop_for_status(tt)
  tmp <- jsonlite::fromJSON(con_utf8(tt))
  tmp <- nmslwr(tmp)
  if (taxonomy) {
    tmp$taxonconcepts <- nmslwr(tmp$taxonconcepts)
    tmp$taxonconcepts$taxonrank <- tolower(tmp$taxonconcepts$taxonrank)
  }
  return(tmp)
}

nmslwr <- function(x) {
  setNames(x, tolower(names(x)))
}
