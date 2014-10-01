#' Parse taxon names using the GBIF name parser.
#' 
#' @import httr plyr jsonlite
#' @param scientificname A character vector of scientific names.
#' @return A \code{data.frame} containing fields extracted from parsed 
#' taxon names. Fields returned are the union of fields extracted from
#' all species names in \code{scientificname}.
#' @author John Baumgartner (johnbb@@student.unimelb.edu.au)
#' @references \url{http://dev.gbif.org/wiki/display/POR/Webservice+API},  
#' \url{http://tools.gbif.org/nameparser/api.do}
#' @seealso \code{\link{gni_parse}}
#' @export
#' @examples \donttest{
#' gbif_parse(scientificname='x Agropogon littoralis')
#' gbif_parse(c('Arrhenatherum elatius var. elatius', 
#'              'Secale cereale subsp. cereale', 'Secale cereale ssp. cereale',
#'              'Vanessa atalanta (Linnaeus, 1758)'))
#' }

gbif_parse <- function(scientificname) {
  url <- "http://api.gbif.org/v1/parser/name"
  tt <- POST(url,
            config=c(add_headers('Content-Type' = 
                                   'application/json')),
            body=jsonlite::toJSON(scientificname))
  stop_for_status(tt)
  res <- content(tt)
  tmp <- do.call(rbind.fill, lapply(res, as.data.frame))
  names(tmp) <- tolower(names(tmp))
  tmp
}
