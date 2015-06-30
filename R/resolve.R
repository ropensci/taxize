#' Resolve names from different data sources
#'
#' Resolve names from iPlant's name resolver, and the Global Names Resolver (GNR)
#'
#' @export
#' @param query Vector of one or more names.
#' @param db Source to check names against. One of iplant or gnr. Default: gnr
#' @param callopts Curl options passed on to \code{\link[httr]{GET}}
#' @param ... Further named args passed on to each respective function. See examples. Note that
#' parameters for specific data sources are specific to those data sources. There is one
#' exception - the \code{callopts} parameter is shared among all
#' data sources, so if you pass that parameter it will influence each data source.
#' @return A list with length equal to length of the db parameter (number of sources requested.)
#' @examples \dontrun{
#' resolve(query=c("Helianthus annuus", "Homo sapiens"))
#' resolve(query="Quercus keloggii", db='gnr')
#' resolve(query="Helianthus annuus", db='gnr', preferred_data_sources = c(3,4))
#' resolve(query=c("Helianthus annuus", "Homo sapiens"), db=c('iplant','gnr'))
#' resolve(query="Quercus keloggii", db=c('iplant','gnr'))
#' library("httr")
#' resolve(query="Qercuss", callopts=verbose())
#' resolve(query=c("Helianthus annuus", "Homo sapiens"), callopts=timeout(3))
#' }

resolve <- function(query, db='gnr', callopts = list(), ...) {
  db <- match.arg(db, choices = c('iplant', 'gnr'), several.ok = TRUE)
  foo <- function(x, y, ...){
    res <- switch(x,
                  gnr = tryDefault(gnr_resolve(names = y, callopts = callopts, ...)),
                  iplant = tryDefault(iplant_resolve(query = y, callopts = callopts, ...)))
    if (is.null(res)) "Error: no data found" else res
  }
  setNames(lapply(db, function(z) foo(z, query, ...)), db)
}

tryDefault <- function(expr, default = NULL, quiet = TRUE) {
  result <- default
  if (quiet) {
    tryCatch(result <- expr, error = function(e) {
    })
  }
  else {
    try(result <- expr)
  }
  result
}
