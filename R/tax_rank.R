#' Get taxonomic rank for a given taxonomic name query.
#' 
#' Specify what database you want to use: itis or ncbi.
#' 
#' @param query character; Vector of taxonomic names to query.
#' @param db character; The database to search from: 'tis', 'ncbi' or 'both'.
#'  If 'both' both NCBI and ITIS will be queried. Result will be the union of both.
#' @param pref If db='both', sets the preference for the union. Either 'ncbi' or 'itis'.
#' @param verbose logical; If TRUE the actual taxon queried is printed on the console.
#' 
#' @return A data.frame with one column for every queried rank.
#' 
#' @examples \dontrun{
s
#' tax_rank(query="Helianthus annuus", db="itis")
#' tax_rank(query="Helianthus annuus", db="ncbi")
#' tax_rank(query="Helianthus, db="itis")
#' 
#' # query both
#' tax_rank(query=c("Helianthus annuus", 'Baetis rhodani'), get=c("genus", "kingdom"), db="both")
#' }
#' @export
tax_rank <- function(query = NULL, get = NULL, db = "itis", pref = 'ncbi', verbose = TRUE)
{
  if(is.null(query))
    stop('Need to specify query!\n')
  if(is.null(get))
    stop('Need to specify get!\n')
  if(!db %in% c('itis', 'ncbi', 'both'))
    stop("db must be one of 'itis', 'ncbi' or 'both'!\n")
  if(db == 'both' & pref %notin% c('ncbi', 'itis'))
    stop("if db=both, pref must be either 'itis' or 'ncbi'!\n")
  
  fun <- function(query, get, db, verbose){
    # ITIS
    if(db == "itis"){
      tsn <- get_tsn(query, searchtype="sciname", verbose = verbose)
      if(is.na(tsn)) {
        if(verbose) 
          message("No TSN found for species '", query, "'!\n")
        out <- data.frame(t(rep(NA, length(get))))
        names(out) <- get
      } else {
        tt <- classification(tsn)[[1]]
        out <- tt[nrow(tt), 'rankName']
        if(length(out) == 0)
          out <- NA
      }
    }
    
    # NCBI
    if(db == "ncbi")	{
      uid <- get_uid(query, verbose = verbose)
      if(is.na(uid)){
        if(verbose) 
          message("No UID found for species '", query, "'!\n")
        out <- data.frame(t(rep(NA, length(get))))
        names(out) <- get
      } else {
        hierarchy <- classification(uid)[[1]]
        out <- hierarchy[nrow(hierarchy), 'Rank']
        if(length(out) == 0)
          out <- NA
      }
    }
    
    # combine both
    # NCBI
    if(db == 'both') {
      uid <- get_uid(query, verbose = verbose)
      if(is.na(uid)){
        if(verbose) 
          message("No UID found for species '", query, "'!\n")
        match_uid <- rep(NA, length(get))
      } else {
        hierarchy <- classification(uid)[[1]]
        match_uid <- hierarchy$ScientificName[match(tolower(get), tolower(hierarchy$Rank))]
      }
      # itis
      tsn <- get_tsn(query, searchtype="sciname", verbose = verbose)
      if(is.na(tsn)) {
        if(verbose) 
          message("No TSN found for species '", query, "'!\n")
        match_tsn <- rep(NA, length(get))
      } else {
        if(tsn=="notsn") {
          out <- "notsn"
        } else {
          tt <- classification(tsn)[[1]]
          match_tsn <- tt$taxonName[match(tolower(get), tolower(tt$rankName))]
        }
      }
      match_both <- ifelse(is.na(match_uid), match_tsn, match_uid)
      out <- data.frame(t(match_both), stringsAsFactors=FALSE)
      names(out) <- get     
    }
    return(out)
  }
  out <- ldply(query, .fun=function(x) fun(query=x, get=get, db=db, verbose=verbose))
  return(out)
}