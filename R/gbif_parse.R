#' Parse taxon names using the GBIF name parser.
#'
#' @export
#' @param scientificname (character) scientific names
#' @param ... Further args passed on to [crul::verb-POST]
#' @return A `data.frame` containing fields extracted from parsed
#' taxon names. Fields returned are the union of fields extracted from
#' all species names in `scientificname`.
#' @author John Baumgartner \email{johnbb@@student.unimelb.edu.au}
#' @references https://www.gbif.org/tools/name-parser/about
#' @seealso [gni_parse()]
#' @examples \dontrun{
#' gbif_parse(scientificname='x Agropogon littoralis')
#' gbif_parse(c('Arrhenatherum elatius var. elatius',
#'              'Secale cereale subsp. cereale', 'Secale cereale ssp. cereale',
#'              'Vanessa atalanta (Linnaeus, 1758)'))
#' }
gbif_parse <- function(scientificname, ...) {
  cli <- crul::HttpClient$new("https://api.gbif.org", 
    headers = c(list('Content-Type' = "application/json"), tx_ual),
    opts = list(...))
  tt <- cli$post("v1/parser/name", body = jsonlite::toJSON(scientificname))
  tt$raise_for_status()
  stopifnot(tt$response_headers$`content-type` == 'application/json')
  res <- jsonlite::fromJSON(tt$parse("UTF-8"), FALSE)
  res <- lapply(res, function(x) Map(function(z) if (is.null(z)) NA else z, x))
  (tmp <- data.table::setDF(
    data.table::rbindlist(res, fill = TRUE, use.names = TRUE)))
  setNames(tmp, tolower(names(tmp)))
}
