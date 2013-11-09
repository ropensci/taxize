#' Retrieve taxonomic rank name from given TSN.
#' 
#' @param query Quoted TSN for a taxonomic group (numeric), or scientific
#' 		name (character).
#' @param verbose logical; If TRUE (default), informative messages printed.
#' @details You can print informative messages by setting supmess=FALSE.
#' @return Taxonomic rank name.
#' @examples \dontrun{
#' itis_taxrank(query=202385)
#' }
#' @export
itis_taxrank <- function(query = NULL, verbose=TRUE)
{
	ldply(query, function(z) gettaxonomicranknamefromtsn(z, verbose=verbose))$rankName
}