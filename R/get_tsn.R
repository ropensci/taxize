#' Get the TSN code for a search term.
#' 
#' Retrieve the taxonomic serial numbers (TSN) of a taxon from ITIS.
#' 
#' @import plyr
#' @param searchterm character; A vector of common or scientific names.
#' @param searchtype character; One of 'sciname', 'comnamebeg', 'comname', 'comnameend'.
#' @param ask logical; should get_tsn be run in interactive mode? 
#' If TRUE and more than one TSN is found for teh species, the user is asked for 
#' input. If FALSE NA is returned for multiple matches.
#' @param verbose logical; should progress be printed?
#' 
#' @return A vector of taxonomic serial numbers (TSN). If a taxon is not 
#'    found NA. If more than one TSN is found the function asks for user input.
#' 		See functions in the \code{itis} function.
#'   	
#'   	
#' @seealso \code{\link[taxize]{get_uid}}, \code{\link[taxize]{classification}}
#' 
#' @export
#' @examples \dontrun{
#' get_tsn(searchterm = "Quercus douglasii", searchtype = "sciname")
#' get_tsn(searchterm = "Chironomus riparius", searchtype = "sciname")
#' get_tsn(c("Chironomus riparius","Quercus douglasii"), "sciname")
#' splist <- c("annona cherimola", 'annona muricata', "quercus robur", 
#' 		"shorea robusta", "pandanus patina", "oryza sativa", "durio zibethinus")
#' get_tsn(splist, "sciname")
#' 
#' # When not found
#' get_tsn("howdy")
#' get_tsn(c("Chironomus riparius", "howdy"))
#' 
#' # Using common names
#' get_tsn(searchterm="black bear", searchtype="comname")
#' get_tsn(searchterm="black", searchtype="comnamebeg")
#' get_tsn(searchterm="bear", searchtype="comnameend")
#' }
get_tsn <- function(searchterm, searchtype = "sciname", ask = TRUE, verbose = TRUE) 
{
  fun <- function(x, searchtype, ask, verbose)
  {
    mssg(verbose, "\nRetrieving data for taxon '", x, "'\n")
#     tsn_df <- searchtype(query=x)
    
    if(searchtype == "sciname"){ tsn_df <- searchbyscientificname(x) } else
    		if(searchtype == "comnamebeg") { tsn_df <- searchbycommonnamebeginswith(x) } else
    			if(searchtype == "comname") { tsn_df <- searchbycommonname(x) } else
    				if(searchtype == "comnameend") { tsn_df <- searchbycommonnameendswith(x) } else
    					stop("searchtype not valid!")
    
    # should return NA if spec not found
    if (nrow(tsn_df) == 0){
      mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
      tsn <- NA
    }
    # take the one tsn from data.frame
    if (nrow(tsn_df) == 1)
      tsn <- tsn_df$tsn
    # check for direct match
    if (nrow(tsn_df) > 1){
      names(tsn_df)[1] <- "target"
      direct <- match(tolower(x), tolower(tsn_df$target))
      if(!is.na(direct))
        tsn <- tsn_df$tsn[direct]
    } else {
      direct <- NA
    }
    # multiple matches
    if (nrow(tsn_df) > 1 & is.na(direct)){
      if(ask) {
        names(tsn_df)[1] <- "target"
        # user prompt
        tsn_df <- tsn_df[order(tsn_df$target), ]
        rownames(tsn_df) <- 1:nrow(tsn_df)
        
        # prompt
        message("\n\n")
        print(tsn_df)
        message("\nMore than one TSN found for taxon '", x, "'!\n
            Enter rownumber of taxon (other inputs will return 'NA'):\n") # prompt
        take <- scan(n = 1, quiet = TRUE, what = 'raw')
        
        if(length(take) == 0)
          take <- 'notake'
        if(take %in% seq_len(nrow(tsn_df))){
          take <- as.numeric(take)
          message("Input accepted, took taxon '", as.character(tsn_df$target[take]), "'.\n")
          tsn <-  tsn_df$tsn[take]
        } else {
          tsn <- NA
          mssg(verbose, "\nReturned 'NA'!\n\n")
        }
      } else {
        tsn <- NA
      }
        
    }
    return(as.character(tsn))
  }
  out <- laply(searchterm, fun, searchtype, ask, verbose)
  class(out) <- "tsn"
  return(out)
}