#' Search UK National Biodiversity Network database for taxonomic classification
#'
#' @export
#'
#' @param id (character) An NBN idientifier.
#' @param ... Further args passed on to \code{\link[httr]{GET}}.
#'
#' @author Scott Chamberlain, \email{myrmecocystus@@gmail.com}
#'
#' @examples \dontrun{
#' nbn_classification(id="NHMSYS0000502940")
#'
#' # get id first, then pass to this fxn
#' id <- get_nbnid("blue tit", rec_only = TRUE, rank = "Species")
#' nbn_classification(id)
#'
#' library('httr')
#' nbn_classification(id="NHMSYS0000502940", config=verbose())
#' }
nbn_classification <- function(id, ...)
{
  url <- sprintf("https://data.nbn.org.uk/api/taxa/%s/taxonomy", id)
  nbn_GET_2(url, ...)
}

nbn_GET_2 <- function(url, ...){
  res <- GET(url, ...)
  stop_for_status(res)
  tt <- content(res, as = "text")
  jsonlite::fromJSON(tt, TRUE)
}
