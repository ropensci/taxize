#' Get taxonomic names for a given taxonomic name query.
#'
#' @param query TSN number (taxonomic serial number).
#' @param get The rank of the taxonomic name to get.
#' @return Taxonomic name for the searched taxon.
#' @examples \donttest{
#' itis_name(query="Helianthus annuus", get="family")
#' }
#' @export
#' @rdname itis_name-deprecated
itis_name <- function(query = NULL, get = NULL)
{
	# 	tsn <- get_tsn(query, searchtype="sciname")
	# 	tt <- getfullhierarchyfromtsn(tsn)
	# 	as.character(tt[tt$rankName == capwords(get, onlyfirst=T), "taxonName"])
	.Deprecated("tax_name", "taxize", msg="This function is deprecated. This function has changed name to tax_name.")
}
