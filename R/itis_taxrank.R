#' Retrieve taxonomic rank name from given TSN.
#' 
#' @param query Quoted TSN for a taxonomic group (numeric), or scientific
#' 		name (character).
#' @details You can print informative messages by setting supmess=FALSE.
#' @return Taxonomic rank name.
#' @examples \dontrun{
#' itis_taxrank(query=202385)
#' }
#' @export
itis_taxrank <- function(query = NULL)
{
# 	as.character(gettaxonomicranknamefromtsn(query)$rankName)
	itis(query=query, searchtype="gettaxonomicranknamefromtsn")$rankName
}