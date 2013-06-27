#' Get rank for a given taxonomic name.
#' 
#' Retrieve rank of a taxon.
#' 
#' @param query character; Vector of taxonomic names to query.
#' @param db character; The database to search from: 'tis', 'ncbi' or 'both'.
#'  If 'both' both NCBI and ITIS will be queried. Result will be the union of both.
#' @param pref If db = 'both', sets the preference for the union. Either 'ncbi' or 'itis'.
#' @param verbose logical; If TRUE the actual taxon queried is printed on the console.
#' 
#' @note While \code{\link[taxize]{tax_name}} returns the name of a specified rank, 
#' \code{\link[taxize]{tax_rank}} returns the actual rank of the taxon.
#' 
#' @return A data.frame with one column for every queried taxon.
#' 
#' @seealso \code{\link[taxize]{classification}}, \code{\link[taxize]{tax_name}}
#' 
#' @examples \dontrun{
#' tax_rank(query = "Helianthus annuus", db = "itis")
#' tax_rank(query = "Helianthus annuus", db = "ncbi")
#' tax_rank(query = "Helianthus, db = "itis")
#' tax_rank(query = "Baetis rhodani", db = "itis")
#' 
#' # query both
#' tax_rank(query=c("Helianthus annuus", 'Baetis rhodani'), db="both")
#' 
#' # An alternative way would be to use \link{classification} and sapply over the list
#' x <- 'Baetis'
#' classi <- classification(get_uid(x))
#' sapply(classi, function(x) x[nrow(x), 'Rank'])
#' }
#' @export
tax_rank <- function(query = NULL, db = "itis", pref = 'ncbi', verbose = TRUE)
{
  if(is.null(query))
    stop('Need to specify query!\n')
  if(!db %in% c('itis', 'ncbi', 'both'))
    stop("db must be one of 'itis', 'ncbi' or 'both'!\n")
  if(db == 'both' & pref %notin% c('ncbi', 'itis'))
    stop("if db=both, pref must be either 'itis' or 'ncbi'!\n")
  
  fun <- function(query, get, db, verbose){
    # ITIS
    if(db == "itis" | db == 'both'){
      tsn <- get_tsn(query, searchtype = "sciname", verbose = verbose)
      if(is.na(tsn)) {
        if(verbose) 
          message("No TSN found for species '", query, "'!\n")
        out_tsn <- NA
      } else {
        tt <- classification(tsn)[[1]]
        out_tsn <- tt[nrow(tt), 'rankName']
        if(length(out_tsn) == 0)
          out_tsn <- NA
      }
    }
    
    # NCBI
    if(db == "ncbi" | db == 'both')	{
      uid <- get_uid(query, verbose = verbose)
      if(is.na(uid)){
        if(verbose) 
          message("No UID found for species '", query, "'!\n")
        out_uid <- NA
      } else {
        hierarchy <- classification(uid)[[1]]
        out_uid <- hierarchy[nrow(hierarchy), 'Rank']
        if(length(out_uid) == 0)
          out_uid <- NA
      }
    }
    
    # combine
    if(db == 'both') {
      out <- ifelse(is.na(out_uid), out_tsn, out_uid)
    }
    if(db == 'ncbi')
      out <- out_uid
    if(db == 'itis')
      out <- out_tsn
    return(tolower(out))
  }
  out <- ldply(query, .fun = function(x) fun(query = x, get = get, db = db, verbose = verbose))
  names(out) <- 'Rank'
  return(out)
}