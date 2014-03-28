#' Resolve names from many different sources
#' 
#' @param query Vector of names
#' @param from Source to check names against. One of iplant, tnrs, or gnr
#' @param ... Further named args passed on to each respective function.
#' @export
#' @examples \dontrun{
#' resolve(query=c("Helianthus annuus", "Homo sapiens"))
#' resolve(query=c("Helianthus annuus", "Homo sapiens"), from='tnrs', source="NCBI")
#' resolve(query=c("Helianthus annuus", "Homo sapiens"), callopts=verbose())
#' resolve(query=c("Helianthus annuus", "Homo sapiens"), callopts=list(verbose=TRUE, timeout.ms=3))
#' }

resolve <- function(query, from='iplant', ...){  
  
  from <- match.arg(from, choices = c('iplant','tnrs','gnr'), several.ok = TRUE)
  
  foo <- function(x, y, ...){
    res <- switch(x, 
                  gnr = trycall(gnr_resolve(names = y, ...)),
                  tnrs = trycall(tnrs(query = y, ...)),
                  iplant = trycall(iplant_resolve(query = y, ...)))
    if(is.null(res)){
      "Error bitch"
    } else {
      # Process output to similar format regardless of source pulled from
      process_sources(x, res)
    }
  }
  output <- lapply(from, function(z) foo(z, query, ...))
  names(output) <- from
  return( output )
}

trycall <- function(x){
  try_default(x, NULL, TRUE)
}

process_sources <- function(x,y){
  gnr_parse <- function(x){
    tmp <- data.frame(downfornow1=NA, downfornow2=NA)
    # re-arrange
    tmp
  }
  tnrs_parse <- function(x){
    tmp <- data.frame(apply(x, 2, function(z) gsub('[\"]', "", z)), stringsAsFactors = FALSE)
    tmp$score <- as.numeric(tmp$score)
    # re-arrange
    tmp
  }
  iplant_parse <- function(x){
    tmp <- do.call(rbind.fill, lapply(x, data.frame, stringsAsFactors = FALSE))
    # re-arrange
    tmp
  }
  switch(x,
         gnr = gnr_parse(y),
         tnrs = tnrs_parse(y),
         iplant = iplant_parse(y))
}