#' Retrieve taxonomic rank name from given TSN.
#' 
#' @import ritis
#' @param query Quoted TSN for a taxonomic group (numeric), or scientific
#' 		name (character).
#' @details You can print informative messages by setting supmess=FALSE.
#' @return Taxonomic rank name.
#' @export
#' @examples \dontrun{
#' itis_gettaxrank(202385)
#' }
itis_gettaxrank <- function(query = NULL)
{
	as.character(gettaxonomicranknamefromtsn(query)$rankName)
}