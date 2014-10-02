#' Return all synonyms for a taxon name with a given id from NBN
#'
#' @export
#' 
#' @param id the taxon identifier code
#' @param ... Further args passed on to \code{\link[httr]{GET}}
#' @return A data.frame
#' @examples \donttest{
#' nbn_synonyms(id = 'NHMSYS0000502940')
#' nbn_synonyms(id = 'NHMSYS0001501147')
#' nbn_synonyms(id = 'NHMSYS0000456036')
#' }

nbn_synonyms <- function(id, ...)
{
  url <- sprintf("https://data.nbn.org.uk/api/taxa/%s/synonyms", id)
  nbn_GET_2(url, ...)
}
