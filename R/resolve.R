#' @title Resolve names from different data sources
#'
#' @description Resolve names from iPlant's name resolver, and 
#' the Global Names Resolver (GNR)
#'
#' @export
#' @param sci Vector of one or more taxonomic names (common names not 
#' supported)
#' @param db Source to check names against. One of iplant or gnr. 
#' Default: gnr. Note that each taxonomic data source has their own 
#' identifiers, so that if you provide the wrong `db` value for the 
#' identifier you could get a result, but it will likely be wrong (not 
#' what you were expecting).
#' @param query Deprecated, see `sci`
#' @param ... Curl options passed on to [crul::verb-GET] or 
#' [crul::verb-POST]. In addition, further named args passed 
#' on to  each respective function. See examples
#' @return A list with length equal to length of the db parameter (number 
#' of sources requested), with each element being a data.frame or list 
#' with results from that source.
#' @examples \dontrun{
#' resolve(sci=c("Helianthus annuus", "Homo sapiens"))
#' resolve(sci="Quercus keloggii", db='gnr')
#' resolve(sci=c("Helianthus annuus", "Homo sapiens"), db=c('iplant', 'gnr'))
#' resolve(sci="Quercus keloggii", db=c('iplant', 'gnr'))
#'
#' # pass in options specific to each source
#' resolve("Helianthus annuus", db = 'gnr', preferred_data_sources = c(3, 4))
#' resolve("Helianthus annuus", db = 'iplant', retrieve = 'best')
#' identical(
#'  resolve("Helianthus annuus", db = 'iplant', retrieve = 'best')$iplant,
#'  iplant_resolve("Helianthus annuus", retrieve = 'best')
#' )
#'
#' # pass in curl options
#' resolve(sci="Qercuss", db = "iplant", verbose = TRUE)
#' }
resolve <- function(sci, db = 'gnr', query = NULL, ...) {
  pchk(query, "sci")
  if (!is.null(query)) sci <- query
  db <- match.arg(db, choices = c('iplant', 'gnr'), 
    several.ok = TRUE)
  foo <- function(x, y, ...){
    res <- switch(x,
      gnr = tryDefault(gnr_resolve(sci = y, ...)),
      iplant = tryDefault(iplant_resolve(sci = y, ...))
    )
    if (is.null(res)) "Error: no data found" else res
  }
  stats::setNames(lapply(db, function(z) foo(z, sci, ...)), db)
}

tryDefault <- function(expr, default = NULL, quiet = TRUE) {
  result <- default
  if (quiet) {
    tryCatch(result <- expr, error = function(e) {
    })
  } else {
    try(result <- expr)
  }
  result
}
