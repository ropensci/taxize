#' Get taxonomic names for a given rank.
#' 
#' Retrieve name of querried taxonomic rank of a taxon.
#' 
#' @param query character; Vector of taxonomic names to query.
#' @param get character; The ranks of the taxonomic name to get, see 
#' \code{\link[taxize]{rank_ref}}.
#' @param db character; The database to search from: 'itis', 'ncbi' or 'both'.
#'  If 'both' both NCBI and ITIS will be queried. Result will be the union of 
#'  both.
#' @param pref character; If db = 'both', sets the preference for the union. 
#' Either 'ncbi' or 'itis'. 
#' Currently not implemented - preference is given to ncbi.
#' @param verbose logical; If TRUE the actual taxon queried is printed on the 
#' console.
#' @param ... Other arguments passed to \code{\link[taxize]{get_tsn}} or \code{\link[taxize]{get_uid}}.
#' 
#' @return A data.frame with one column for every queried rank.
#' 
#' @note While \code{\link[taxize]{tax_rank}} returns the actual rank of a 
#' taxon,\code{\link[taxize]{tax_name}} searches and returns any specified rank 
#' higher in taxonmy.
#' 
#' @seealso \code{\link[taxize]{classification}}
#' 
#' @examples \dontrun{
#' # A case where itis and ncbi use the same names
#' tax_name(query = "Helianthus annuus", get = "family", db = "itis")
#' tax_name(query = "Helianthus annuus", get = "family", db = "ncbi")
#' 
#' # Case where itis and ncbi use different names
#' tax_name(query = "Helianthus annuus", get = "kingdom", db = "itis")
#' tax_name(query = "Helianthus annuus", get = "kingdom", db = "ncbi")
#' 
#' # multiple get arguments
#' tax_name(query = c("Helianthus annuus","Baetis rhodani"), get = c("genus", 
#' "kingdom"), db = "ncbi")
#' tax_name(query = "xxx", get = c("genus", "kingdom"), db = "itis")
#' 
#' # query both
#' tax_name(query=c("Helianthus annuus", 'Baetis rhodani'), get=c("genus", 
#' "kingdom"), db="both")
#' }
#' @export
tax_name <- function(query = NULL, get = NULL, db = "itis", pref = 'ncbi', 
                     verbose = TRUE, ...)
{
  if(is.null(query))
    stop('Need to specify query!\n')
  if(is.null(get))
    stop('Need to specify get!\n')
  if(!db %in% c('itis', 'ncbi', 'both'))
    stop("db must be one of 'itis', 'ncbi' or 'both'!\n")
  if(db == 'both' & !pref %in% c('ncbi', 'itis'))
    stop("if db=both, pref must be either 'itis' or 'ncbi'!\n")
  
  fun <- function(query, get, db, verbose, ...){
    # ITIS
  	if(db == "itis"){
  		tsn <- get_tsn(query, searchtype = "sciname", verbose = verbose, ...)
      if(is.na(tsn)) {
        if(verbose) 
          message("No TSN found for species '", query, "'!\n")
        out <- data.frame(t(rep(NA, length(get))))
        names(out) <- get
      } else {
        tt <- classification(tsn, ...)[[1]]
        out <- as.character(tt[tolower(tt$rankName) %in% tolower(get), "taxonName"])
        if(length(out) == 0)
          out <- data.frame(t(rep(NA, length(get))))
        out <- data.frame(t(out), stringsAsFactors = FALSE)
        names(out) <- get
      }
  	}
    
    # NCBI
  	if(db == "ncbi")	{
  		uid <- get_uid(query, verbose = verbose, ...)
      if(is.na(uid)){
        if(verbose) 
          message("No UID found for species '", query, "'!\n")
        out <- data.frame(t(rep(NA, length(get))))
        names(out) <- get
      } else {
  			hierarchy <- classification(uid, ...)[[1]]
  			match <- hierarchy$ScientificName[match(tolower(get), tolower(hierarchy$Rank))]
        out <- data.frame(t(match), stringsAsFactors=FALSE)
        names(out) <- get
  		}
  	}
    
    # combine both
      # NCBI
    if(db == 'both') {
      uid <- get_uid(query, verbose = verbose, ...)
      if(is.na(uid)){
        if(verbose) 
          message("No UID found for species '", query, "'!\n")
        match_uid <- rep(NA, length(get))
      } else {
        hierarchy <- classification(uid, ...)[[1]]
        match_uid <- hierarchy$ScientificName[match(tolower(get), tolower(hierarchy$Rank))]
      }
      # itis
      tsn <- get_tsn(query, searchtype="sciname", verbose = verbose, ...)
      if(is.na(tsn)) {
        if(verbose) 
          message("No TSN found for species '", query, "'!\n")
        match_tsn <- rep(NA, length(get))
      } else {
        tt <- classification(tsn)[[1]]
        match_tsn <- tt$taxonName[match(tolower(get), tolower(tt$rankName))]
      }
      match_both <- ifelse(is.na(match_uid), match_tsn, match_uid)
      out <- data.frame(t(match_both), stringsAsFactors = FALSE)
      names(out) <- get     
    }
    return(out)
  }
  out <- ldply(query, .fun = fun, get, db, verbose, ...)
  return(out)
}