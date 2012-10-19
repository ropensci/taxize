#' Get family names to make Phylomatic input object.
#' 
#' @import XML RCurl plyr
#' @param x Quoted tsn number (taxonomic serial number).
#' @return Family name for the searched taxon.
#' @export
#' @examples \dontrun{
#' itis_get_name(query="Helianthus annuus", get="family")
#' }
itis_get_name <- function(query = NULL, get = NULL) 
{
	tsn <- get_tsn(query, searchtype="sciname")
	tt <- getfullhierarchyfromtsn(tsn)
	as.character(tt[tt$rankName == capwords(get, onlyfirst=T), "rankName"])
}