#' Search UK National Biodiversity Network database for
#' taxonomic classification
#'
#' @export
#' @param id (character) An NBN identifier.
#' @param ... Further args passed on to [crul::verb-GET]
#' @return A data.frame
#' @family nbn
#' @author Scott Chamberlain, \email{myrmecocystus@@gmail.com}
#' @references <https://api.nbnatlas.org/>
#' @examples \dontrun{
#' nbn_classification(id="NHMSYS0000376773")
#'
#' # get id first, then pass to this fxn
#' id <- get_nbn("Zootoca vivipara", rec_only = TRUE, rank = "Species")
#' nbn_classification(id)
#'
#' nbn_classification(id="NHMSYS0000502940", verbose = TRUE)
#' }
nbn_classification <- function(id, ...) {
  url <- file.path(nbn_base(), "classification", id)
  nbn_GET_2(url, ...)
}

nbn_GET_2 <- function(url, ...) {
  cli <- crul::HttpClient$new(url, headers = tx_ual, opts = list(...))
  res <- cli$get()
  res$raise_for_status()
  nmslwr(jsonlite::fromJSON(res$parse("UTF-8"), TRUE))
}
