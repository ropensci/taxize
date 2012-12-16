#' Get taxonomic names for a given taxonomic name query.
#' 
#' Specify what database you want to use: itis or ncbi.
#' 
#' @import NCBI2R
#' @param query Taxonomic name (character).
#' @param get The rank of the taxonomic name to get (character). 
#' @param db The database to search from (character).
#' @return Taxonomic name for the searched taxon.
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
tax_name <- function(query = NULL, get = NULL, db = "itis")
{
	if(db=="itis"){
		tsn <- get_tsn(query, searchtype="sciname")
		tt <- getfullhierarchyfromtsn(tsn)
		as.character(tt[tt$rankName == capwords(get, onlyfirst=T), "taxonName"])
	} else
		if(db=="ncbi")	{
			taxid <- GetTax(query)
			hierarchy <- GetTaxInfo(taxid)$lineage
			as.character(hierarchy[hierarchy$line.rank %in% get, "line.sciName"])	
		} else
			stop("db must be one of itis or ncbi")
}