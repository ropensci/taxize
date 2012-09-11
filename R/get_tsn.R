#' Get the TSN code for a search term.
#' 
#' \code{get_tsn} uses a variety of functions from the \code{ritis} package: 
#'   \link{https://github.com/ropensci/ritis}.
#' 
#' @import ritis plyr
#' @param searchterm A vector of common or scientific names.
#' @param searchtype One of 'sciname', 'anymatch', 'comnamebeg', 'comname', 
#'    'comnameend'.
#' @return A vector of taxonomic serial numbers (TSN). If a species is not found NA. 
#' If more than one TSN is found the first hit is taken and a error-message printed.
#' See functions in \code{ritis}.
#' @export
#' @examples \dontrun{
#' get_tsn("Quercus douglasii", "sciname")
#' get_tsn(searchterm="Chironomus riparius", searchtype="sciname")
#' get_tsn(searchterm="polar bear", searchtype="comname")
#' get_tsn(c("Chironomus riparius","Quercus douglasii"), "sciname")
#' get_tsn(c("aa aa", "Chironomus riparius"), searchtype="sciname")
#' splist <- c("annona cherimola", 'annona muricata', "quercus robur", 
#' "shorea robusta", "pandanus patina", "oryza sativa", "durio zibethinus")
#' get_tsn(splist,"sciname")
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
    if (length(tsn) > 1){
      cat("\n
          More than one TSN found for species '", x, "'!\n
          Enter rowname of species to take:\n") # prompt
      print(tsn_df)
      take <- scan(n = 1, quiet = TRUE)
      if(take %in% seq_along(nrow(tsn_df))){
        cat("Input accepted, took species '", as.character(tsn_df$combinedname[take]), "'\n")
      } else {
        stop("Non valid input!")
      }
      tsn <- tsn[take]
    }
    tsn
  }
  out <- laply(searchterm, fun)
  class(out) <- "tsn"
  out
}