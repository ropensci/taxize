#' Resolve names from many different sources
#' 
#' Resolve names from iPlant's name resolver, the Taxonomic Name Resolution Service (TNRS), the 
#' Global Names Resolver (GNR)
#' 
#' @export
#' 
#' @param query Vector of one or more names.
#' @param db Source to check names against. One of iplant, tnrs, or gnr
#' @param callopts Curl options passed on to httr::GET
#' @param ... Further named args passed on to each respective function. See examples. Note that 
#' parameters for specific data sources are specific to those data sources. E.g. you can pass the 
#' parameter \code{source} when using \code{db="tnrs"}, but is meaningless when passed when 
#' \code{db="gnr"}. There is one exception - the \code{callopts} parameter is shared among all 
#' data sources, so if you pass that parameter it will influence each data source. 
#' @return A list with length equal to length of the db parameter (number of sources requested.)
#' @examples \donttest{
#' resolve(query=c("Helianthus annuus", "Homo sapiens"))
#' resolve(query=c("Helianthus annuus", "Homo sapiens"), db='tnrs', source="iPlant_TNRS")
#' resolve(query="Quercus keloggii", db='gnr')
#' resolve(query="Helianthus annuus", db='gnr', preferred_data_sources = c(3,4))
#' resolve(query=c("Helianthus annuus", "Homo sapiens"), db=c('iplant','gnr'))
#' resolve(query="Quercus keloggii", db=c('iplant','gnr','tnrs'))
#' library("httr")
#' resolve(query="Qercuss", callopts=verbose())
#' resolve(query=c("Helianthus annuus", "Homo sapiens"), callopts=timeout(3))
#' }

resolve <- function(query, db='iplant', callopts=list(), ...){  
  
  db <- match.arg(db, choices = c('iplant','tnrs','gnr'), several.ok = TRUE)
  
  foo <- function(x, y, ...){
    res <- switch(x, 
                  gnr = try_default(gnr_resolve(names = y, callopts=callopts, ...)),
                  tnrs = try_default(tnrs(query = y, callopts=callopts, ...)),
                  iplant = try_default(iplant_resolve(query = y, callopts=callopts, ...)))
    if(is.null(res)) "Error: no data found" else res
  }
  output <- lapply(db, function(z) foo(z, query, ...))
  names(output) <- db
  return( output )
}

try_default <- function(expr, default = NULL, quiet = TRUE) 
{
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

# process_sources <- function(x,y){
#   gnr_parse <- function(x){
# #     tmp <- data.frame(downfornow1=NA, downfornow2=NA)
#     # re-arrange
#     x
#   }
#   tnrs_parse <- function(x){
#     tmp <- data.frame(t(apply(x, 2, function(z) gsub('[\"]', "", z))), stringsAsFactors = FALSE)
#     tmp$score <- as.numeric(tmp$score)
#     # re-arrange
#     tmp
#   }
#   iplant_parse <- function(z){
# #     tmp <- do.call(rbind.fill, lapply(z, data.frame, stringsAsFactors = FALSE))
#     # re-arrange
#     z
#   }
#   switch(x,
#          gnr = gnr_parse(y),
#          tnrs = tnrs_parse(y),
#          iplant = iplant_parse(y))
# }
