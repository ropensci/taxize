#' Return all synonyms for a taxon name with a given id from NBN
#'
#' @export
#' @param id the taxon identifier code
#' @param ... Further args passed on to [crul::verb-GET]
#' @return A data.frame
#' @family nbn
#' @references <https://api.nbnatlas.org/>
#' @examples \dontrun{
#' nbn_synonyms(id = 'NHMSYS0001501147')
#' nbn_synonyms(id = 'NHMSYS0000456036')
#'
#' # none
#' nbn_synonyms(id = 'NHMSYS0000502940')
#' }
nbn_synonyms <- function(id, ...) {
  url <- file.path(nbn_base(), "species", id)
  df <- nbn_GET_2(url, ...)
  df$synonyms
}
