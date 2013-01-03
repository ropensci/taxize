#' Get the TSN code for a search term.
#' 
#' \code{get_tsn} uses a variety of functions from the \code{itis} function.
#' 
#' @import plyr
#' @param searchterm A vector of common or scientific names.
#' @param searchtype One of 'sciname', 'anymatch', 'comnamebeg', 'comname', 
#'    'comnameend'.
#' @param verbose should progress be printed?
#' @return A vector of taxonomic serial numbers (TSN). If a species is not found NA. 
#' 		If more than one TSN is found the function asks for user input.
#' 		See functions in the \code{itis} function.
#' @export
#' @examples \dontrun{
#' get_tsn(searchterm="Quercus douglasii", searchtype="sciname")
#' get_tsn(searchterm="Chironomus riparius", searchtype="sciname")
#' get_tsn(searchterm="polar bear", searchtype="comname")
#' get_tsn(c("Chironomus riparius","Quercus douglasii"), "sciname")
#' get_tsn(c("aa aa", "Chironomus riparius"), searchtype="sciname")
#' splist <- c("annona cherimola", 'annona muricata', "quercus robur", 
#' "shorea robusta", "pandanus patina", "oryza sativa", "durio zibethinus")
#' get_tsn(splist,"sciname")
#' }
get_tsn <- function (searchterm, searchtype = "sciname", verbose = TRUE) 
{
  # fetch itis function from args
	searchtype <- if(searchtype == "sciname"){ "searchbyscientificname" } else
                  if(searchtype == "anymatch") { "searchforanymatch" } else
                    if(searchtype == "comnamebeg") { "searchbycommonnamebeginswith" } else
                      if(searchtype == "comname") { "searchbycommonname" } else
                        if(searchtype == "comnameend") { "searchbycommonnameendswith" } else
                          stop("searchtype not valid!")
  fun <- function(x, verbose) 
  {
    if(verbose)
      cat("\nRetrieving data for species '", x, "'\n")
    tsn_df <- do.call(itis, list(query=x, searchtype=searchtype))[[1]]
    
    # should return NA if spec not found
    if (nrow(tsn_df) == 0)
      tsn <- NA
    # take the one tsn from data.frame
    if (nrow(tsn_df) == 1)
      tsn <- tsn_df$tsn
    # check for direct match
    if (nrow(tsn_df) > 1){
      direct <- match(tolower(x), tolower(tsn_df$combinedname))
      if(!is.na(direct))
        tsn <- tsn_df$tsn[direct]
    } else {
      direct <- NA
    }
    # user prompt
    if (nrow(tsn_df) > 1 & is.na(direct)){
      cat("\n\n")
      print(tsn_df)
      cat("\nMore than one TSN found for species '", x, "'!\n
          Enter rownumber of species (other inputs will return 'NA'):\n") # prompt
      take <- scan(n = 1, quiet = TRUE, what = 'raw')
      if(take %in% seq_len(nrow(tsn_df))){
        take <- as.numeric(take)
        cat("Input accepted, took species '", as.character(tsn_df$combinedname[take]), "'.\n")
      } else {
        tsn <- NA
        cat("Returned 'NA'")
      }
      tsn <-  tsn_df$tsn[take]
    }
    return(as.character(tsn))
  }
  out <- laply(searchterm, fun, verbose)
  class(out) <- "tsn"
  out
}