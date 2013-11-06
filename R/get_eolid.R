#' Get the EOL ID from Encyclopedia of Life from taxonomic names.
#' 
#' @import plyr RCurl
#' @param sciname character; scientific name.
#' @param verbose logical; If TRUE the actual taxon queried is printed on the 
#'    console.
#' 
#' @return A vector of unique identifiers (EOL). If a taxon is not found NA. 
#' If more than one ID is found the function asks for user input. 
#' 
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{get_uid}},
#' \code{\link[taxize]{get_tpsid}}
#' 
#' @export
#' @author Scott Chamberlain, \email{myrmecocystus@@gmail.com}
#' 
#' @examples \dontrun{
#' get_eolid(sciname='Poa annua')
#' get_eolid(sciname='Pinus contorta')
#' 
#' get_eolid(c("Poa annua", "Pinus contorta"))
#' 
#' # When not found
#' get_eolid(sciname="uaudnadndj")
#' get_eolid(c("Chironomus riparius", "uaudnadndj"))
#' }
get_eolid <- function(sciname, verbose = TRUE){
  fun <- function(sciname) {
    if(verbose)
      message("\nRetrieving data for taxon '", sciname, "'\n")
    df <- eol_search(terms=sciname)
    if(nrow(df) == 0){
      message("Not found. Consider checking the spelling or alternate classification")
      id <- "not found"
    } else
    {  
      df <- df[,c('id','title')]
      names(df) <- c('eolid','name')
      id <- df$eolid
    }
    
    # not found on eol
    if(length(id) == 0){
      message("Not found. Consider checking the spelling or alternate classification")
      id <- "not found"
    }
    # more than one found on eol -> user input
    if(length(id) > 1){
      rownames(df) <- 1:nrow(df)
      # prompt
      message("\n\n")
      message("\nMore than one eolid found for taxon '", sciname, "'!\n
          Enter rownumber of taxon (other inputs will return 'NA'):\n")      
      print(df)
      take <- scan(n = 1, quiet = TRUE, what = 'raw')
      
      if(length(take) == 0)
        take <- 'notake'
      if(take %in% seq_len(nrow(df))){
        take <- as.numeric(take)
        message("Input accepted, took eolid '", as.character(df$eolid[take]), "'.\n")
        id <- as.character(df$eolid[take])
      } else {
        id <- NA
        message("\nReturned 'NA'!\n\n")
      }
    }  
    return(id)
  }
  out <- laply(sciname, fun)
  class(out) <- "eolid"
  return(out)
}