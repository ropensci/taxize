#' Get the TSN code for a search term.
#' 
#' \code{get_tsn} uses a variety of functions from the \code{ritis} package: 
#'   \link{https://github.com/ropensci/ritis}.
#' 
#' @import ritis plyr
#' @param searchterm A vector of common or scientific names.
#' @param searchtype One of 'sciname', 'anymatch', 'comnamebeg', 'comname', 
#'    'comnameend'.
#' @return A vector of taxonomic serial numbers (TSN). If a species is not found NA. See functions in \code{ritis}.
#' @export
#' @examples \dontrun{
#' get_tsn("Quercus douglasii", "sciname")
#' get_tsn(searchterm="Chironomus riparius", searchtype="sciname")
#' get_tsn(searchterm="polar bear", searchtype="comname")
#' get_tsn(c("Chironomus riparius","Quercus douglasii"), "sciname")
#' get_tsn(c("aa aa", "Chironomus riparius"), searchtype="sciname")
#' }
get_tsn <- function (searchterm, searchtype) 
{
  # fetch ritis function from args
  ritis_func <- if(searchtype == "sciname"){ "searchbyscientificname" } else
                  if(searchtype == "anymatch") { "searchforanymatch" } else
                    if(searchtype == "comnamebeg") { "searchbycommonnamebeginswith" } else
                      if(searchtype == "comname") { "searchbycommonname" } else
                        if(searchtype == "comnameend") { "searchbycommonnameendswith" } else
                          stop("searchtype not valid!")
  fun <- function(x) 
  {
    tsn_df <- do.call(ritis_func, list(x))
    tsn <- as.character(tsn_df$tsn)
    # should return NA if spec not found
    if (length(tsn) == 0)
      tsn <- NA
    tsn
  }
  out <- laply(searchterm, fun)
  class(out) <- "tsn"
  out
}