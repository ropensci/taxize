#' Retrieve taxonomic rank name from given TSN.
#' 
#' @param query Quoted TSN for a taxonomic group (numeric), or scientific
#' 		name (character).
#' @param locally If TRUE, queries are run locally in sqlite3; if FALSE (the default), 
#' 		queries are run against the ITIS web API. locally=TRUE should be faster in almost all cases.
#' @details You can print informative messages by setting supmess=FALSE.
#' @return Taxonomic rank name.
#' @examples \dontrun{
#' itis_taxrank(query=202385)
#' itis_taxrank(query=202385, locally=TRUE)
#' }
#' @export
itis_taxrank <- function(query = NULL, locally = FALSE)
{
# 	searchtype <- "gettaxonomicranknamefromtsn"
# 	as.character(gettaxonomicranknamefromtsn(query)$rankName)
# 	itis(query=query, searchtype="gettaxonomicranknamefromtsn")$rankName	
	ldply(query, gettaxonomicranknamefromtsn, locally = locally)$rankName
}