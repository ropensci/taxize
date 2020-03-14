#' @title Resolve names from different data sources
#'
#' @description Resolve names from iPlant's name resolver, the Taxonomic 
#' Name Resolution Service (TNRS), and the Global Names Resolver (GNR)
#'
#' @export
#' @param query Vector of one or more taxonomic names (common names not 
#' supported)
#' @param db Source to check names against. One of iplant, tnrs, or gnr. 
#' Default: gnr. Note that each taxonomic data source has their own 
#' identifiers, so that if you provide the wrong `db` value for the 
#' identifier you could get a result, but it will likely be wrong (not 
#' what you were expecting).
#' @param ... Curl options passed on to [crul::verb-GET] or 
#' [crul::verb-POST]. In addition, further named args passed 
#' on to  each respective function. See examples
#' @return A list with length equal to length of the db parameter (number 
#' of sources requested), with each element being a data.frame or list 
#' with results from that source.
#' @examples \dontrun{
#' resolve(query=c("Helianthus annuus", "Homo sapiens"))
#' resolve(query="Quercus keloggii", db='gnr')
#' resolve(query=c("Helianthus annuus", "Homo sapiens"), db='tnrs')
#' resolve(query=c("Helianthus annuus", "Homo sapiens"), db=c('iplant', 'gnr'))
#' resolve(query="Quercus keloggii", db=c('iplant', 'gnr'))
#' resolve(query="Quercus keloggii", db=c('iplant', 'gnr', 'tnrs'))
#'
#' # pass in options specific to each source
#' resolve("Helianthus annuus", db = 'gnr', preferred_data_sources = c(3, 4))
#' resolve("Helianthus annuus", db = 'iplant', retrieve = 'best')
#' identical(
#'  resolve("Helianthus annuus", db = 'iplant', retrieve = 'best')$iplant,
#'  iplant_resolve("Helianthus annuus", retrieve = 'best')
#' )
#' mynames <- c("Helianthus annuus", "Pinus contorta", "Poa annua",
#'    "Abies magnifica", "Rosa california")
#' resolve(mynames, db = 'tnrs', source = "NCBI")
#' resolve(mynames, db = 'tnrs', source = "iPlant_TNRS")
#' identical(
#'  resolve(mynames, db = 'tnrs', source = "iPlant_TNRS")$tnrs,
#'  tnrs(mynames, source = "iPlant_TNRS")
#' )
#'
#' # pass in curl options
#' resolve(query="Qercuss", db = "iplant", verbose = TRUE)
#' }
resolve <- function(query, db = 'gnr', ...) {
  db <- match.arg(db, choices = c('iplant', 'gnr', 'tnrs'), 
    several.ok = TRUE)
  foo <- function(x, y, ...){
    res <- switch(x,
      gnr = tryDefault(gnr_resolve(names = y, ...)),
      tnrs = tryDefault(tnrs(query = y, ...)),
      iplant = tryDefault(iplant_resolve(query = y, ...))
    )
    if (is.null(res)) "Error: no data found" else res
  }
  stats::setNames(lapply(db, function(z) foo(z, query, ...)), db)
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
