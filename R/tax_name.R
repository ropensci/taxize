#' Get taxonomic names for a given taxonomic name query.
#' 
#' Specify what database you want to use: itis or ncbi.
#' 
#' @import NCBI2R
#' @param query Taxonomic name (character).
#' @param get The rank of the taxonomic name to get (character). 
#' @param db The database to search from (character).
#' @param verbose logical; If TRUE the actual taxon queried is printed on the console.
#' @return Taxonomic name for the searched taxon. If the taxon is not found NA is returned.
#' @examples \dontrun{
#' # A case where itis and ncbi use the same names
#' tax_name(query="Helianthus annuus", get="family", db="itis")
#' tax_name(query="Helianthus annuus", get="family", db="ncbi")
#' 
#' # Case where itis and ncbi use different names
#' tax_name(query="Helianthus annuus", get="kingdom", db="itis")
#' tax_name(query="Helianthus annuus", get="kingdom", db="ncbi")
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
	if(db=="itis"){
		tsn <- get_tsn(query, searchtype="sciname", verbose = verbose)
    if(is.na(tsn)) {
      return(NA)
    } else {
      tt <- getfullhierarchyfromtsn(tsn)
      out <- as.character(tt[tt$rankName == capwords(get, onlyfirst=T), "taxonName"])
      return(out)
    }
	} else
		if(db=="ncbi")	{
			uid <- get_uid(query, verbose = verbose)
      if(is.na(uid)){
        return(NA)
      } else {
  			hierarchy <- classification(uid)[[1]]
  			out <- as.character(hierarchy[hierarchy$Rank %in% get, "ScientificName"])	
        return(out)
  			}
		} else
			stop("db must be one of itis or ncbi")
}