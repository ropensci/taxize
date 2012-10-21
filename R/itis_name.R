#' Get family names to make Phylomatic input object.
#' 
#' @import XML RCurl plyr
#' @param query TSN number (taxonomic serial number).
#' @param get The rank of the taxonomic name to get. See data(rank_ref) for
#' 		possible names.
#' @return Taxonomic name for the searched taxon.
#' @examples \dontrun{
#' itis_name(query="Helianthus annuus", get="family")
#' }
#' @export
itis_name <- function(query = NULL, get = NULL) 
{
	tsn <- get_tsn(query, searchtype="sciname")
	tt <- getfullhierarchyfromtsn(tsn)
	as.character(tt[tt$rankName == capwords(get, onlyfirst=T), "rankName"])
}