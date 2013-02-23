#' Get taxonomic names for a given taxonomic name query.
#' 
#' Specify what database you want to use: itis or ncbi.
#' 
#' @param query character; Taxonomic name. Needs to of length 1!
#' @param get character; The ranks of the taxonomic name to get. 
#' @param db character; The database to search from: 'tis' or 'ncbi'.
#' @param verbose logical; If TRUE the actual taxon queried is printed on the console.
#' 
#' @return A data.frame with one column for every queried rank.
#' 
#' @examples \dontrun{
#' # A case where itis and ncbi use the same names
#' tax_name(query="Helianthus annuus", get="family", db="itis")
#' tax_name(query="Helianthus annuus", get="family", db="ncbi")
#' 
#' # Case where itis and ncbi use different names
#' tax_name(query="Helianthus annuus", get="kingdom", db="itis")
#' tax_name(query="Helianthus annuus", get="kingdom", db="ncbi")
#' 
#' # multiple get arguments
#' tax_name(query="Helianthus annuus", get=c("genus", "kingdom"), db="ncbi")
#' tax_name(query="xxx", get=c("genus", "kingdom"), db="itis")
#' }
#' @export
tax_name <- function(query = NULL, get = NULL, db = "itis", verbose = TRUE)
{
  if(is.null(query))
    stop('Need to specify query!')
  if(is.null(get))
    stop('Need to specify get!')
  if(length(query) > 1)
    stop('Currently only one species per call allowed!')
  if(!db %in% c('itis', 'ncbi'))
    stop("db must be one of 'itis' or 'ncbi'")
  
  # ITIS
	if(db == "itis"){
		tsn <- get_tsn(query, searchtype="sciname", verbose = verbose)
    if(is.na(tsn)) {
      if(verbose) cat("No TSN found for species '", query, "'!\n")
      out <- data.frame(t(rep(NA, length(get))))
      names(out) <- get
    } else
    	if(tsn=="notsn") {
    		out <- "notsn"
    	} else
    		{
    			tt <- classification(tsn)[[1]]
          match <- tt$taxonName[match(tolower(get), tolower(tt$rankName))]
    			out <- data.frame(t(match), stringsAsFactors=FALSE)
    			names(out) <- get
    		}
	}
  
  # NCBI
	if(db == "ncbi")	{
		uid <- get_uid(query, verbose = verbose)
    if(is.na(uid)){
      if(verbose) cat("No UID found for species '", query, "'!\n")
      out <- data.frame(t(rep(NA, length(get))))
      names(out) <- get
    } else {
			hierarchy <- classification(uid)[[1]]
			match <- hierarchy$ScientificName[match(tolower(get), tolower(hierarchy$Rank))]
      out <- data.frame(t(match), stringsAsFactors=FALSE)
      names(out) <- get
		}
	}
  return(out)
}