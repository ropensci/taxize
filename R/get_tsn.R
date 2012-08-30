#' Get the TSN code for a search term.
#' 
#' \code{get_tsn} uses a variety of functions from the \code{ritis} package: 
#' 		\link{https://github.com/ropensci/ritis}.
#' 
#' @import ritis
#' @param searchterm Any common or scientific name.
#' @param searchtype One of 'sciname', 'anymatch', 'comnamebeg', 'comname', 
#'    'comnameend', 'itistermscomname', 'itistermssciname', or
#'    'tsnsvernacular', 'tsnfullhir', 'tsnhirdown' .
#' @return A taxonomic serial number (TSN), and scientific or common name, depending
#' 		on the searchtype parameter value used. See functions in \code{ritis}.
#' @export
#' @examples \dontrun{
#' get_tsn("Quercus douglasii", "sciname")
#' get_tsn(searchterm="Chironomus riparius", searchtype="sciname")
#' get_tsn(searchterm="polar bear", searchtype="comname")
#' lapply(c("Chironomus riparius","Quercus douglasii"), get_tsn, searchtype="sciname")
#' }
get_tsn <- function(searchterm, searchtype) 
{
	if(searchtype == "sciname"){ searchbyscientificname(searchterm) }	else
		if(searchtype == "anymatch") { searchforanymatch(searchterm) } else
			if(searchtype == "comnamebeg") { searchbycommonnamebeginswith(searchterm) } else
		 		if(searchtype == "comname") { searchbycommonname(searchterm) } else
		 			if(searchtype == "comnameend") { searchbycommonnameendswith(searchterm) } else
		 				if(searchtype == "itistermscomname") { getitistermsfromcommonname(searchterm) } else
		 					if(searchtype == "itistermssciname") { getitistermsfromscientificname(searchterm) } else
		 						if(searchtype == "tsnsvernacular") { gettsnbyvernacularlanguage(searchterm) } else
		 							if(searchtype == "tsnfullhir") { getfullhierarchyfromtsn(searchterm) } else
		 							  stop("searchtype not valid!")
}