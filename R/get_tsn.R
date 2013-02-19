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
#' get_tsn(c("Chironomus riparius","Quercus douglasii"), "sciname")
#' splist <- c("annona cherimola", 'annona muricata', "quercus robur", 
#' 		"shorea robusta", "pandanus patina", "oryza sativa", "durio zibethinus")
#' get_tsn(splist,"sciname")
#' }
get_tsn <- function (searchterm, searchtype = "sciname", verbose = TRUE) 
{
  fun <- function(x, verbose)
  {
    if(verbose)
      cat("\nRetrieving data for species '", x, "'\n")
#     tsn_df <- searchtype(query=x)
    
    if(searchtype == "sciname"){ tsn_df <- searchbyscientificname(x) } else
    	if(searchtype == "anymatch") { tsn_df <- searchforanymatch(x) } else
    		if(searchtype == "comnamebeg") { tsn_df <- searchbycommonnamebeginswith(x) } else
    			if(searchtype == "comname") { tsn_df <- searchbycommonname(x) } else
    				if(searchtype == "comnameend") { tsn_df <- searchbycommonnameendswith(x) } else
    					stop("searchtype not valid!")
#     tsn_df <- ldply(x, searchtype)
    
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
      # sort alphabetically
      tsn_df <- tsn_df[order(tsn_df$combinedname), ]
      rownames(tsn_df) <- 1:nrow(tsn_df)
      
      # prompt
      cat("\n\n")
      print(tsn_df)
      cat("\nMore than one TSN found for species '", x, "'!\n
          Enter rownumber of species (other inputs will return 'NA'):\n") # prompt
      take <- scan(n = 1, quiet = TRUE, what = 'raw')
      
      if(length(take) == 0)
        take <- 'notake'
      if(take %in% seq_len(nrow(tsn_df))){
        take <- as.numeric(take)
        cat("Input accepted, took species '", as.character(tsn_df$combinedname[take]), "'.\n")
        tsn <-  tsn_df$tsn[take]
      } else {
        tsn <- NA
        cat("\nReturned 'NA'!\n\n")
      }
    }
    return(as.character(tsn))
  }
  out <- laply(searchterm, fun, verbose)
  if(nchar(out)==0){out <- "notsn"} else {NULL}
  class(out) <- "tsn"
  return(out)
}