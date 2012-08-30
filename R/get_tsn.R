#' Get the TSN code for a search term.
#' 
#' \code{get_tsn} uses a variety of functions from the \code{ritis} package: 
#' 		\link{https://github.com/ropensci/ritis}.
#' 
#' @import ritis
#' @import plyr
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
get_tsn <- function (searchterm, searchtype) 
{
  ritis_func <- if(searchtype == "sciname"){ "searchbyscientificname" }  else
                  if(searchtype == "anymatch") { "searchforanymatch" } else
                    if(searchtype == "comnamebeg") { "searchbycommonnamebeginswith" } else
                      if(searchtype == "comname") { "searchbycommonname" } else
                        if(searchtype == "comnameend") { "searchbycommonnameendswith" } else
                          if(searchtype == "itistermscomname") { "getitistermsfromcommonname" } else
                            if(searchtype == "itistermssciname") { "getitistermsfromscientificname" } else
                              if(searchtype == "tsnsvernacular") { "gettsnbyvernacularlanguage" } else
                                if(searchtype == "tsnfullhir") { "getfullhierarchyfromtsn" } else
                                  stop("searchtype not valid!")
  if (length(searchterm) == 1) {
    out <- do.call(ritis_func, list(searchterm))
  } 
  else {
    out <- ldply(searchterm, function(x) do.call(ritis_func, list(x)))
  }
  out
}