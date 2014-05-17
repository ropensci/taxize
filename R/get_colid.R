#' Get the Catalogue of Life ID from taxonomic names.
#' 
#' @import plyr RCurl
#' @param sciname character; scientific name.
#' @param ask logical; should get_colid be run in interactive mode? 
#' If TRUE and more than one ID is found for the species, the user is asked for 
#' input. If FALSE NA is returned for multiple matches.
#' @param verbose logical; If TRUE the actual taxon queried is printed on the 
#'    console.
#' 
#' @return A vector of unique identifiers. If a taxon is not found NA. 
#' If more than one ID is found the function asks for user input. 
#' 
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{get_uid}},
#' \code{\link[taxize]{get_tpsid}}, \code{\link[taxize]{get_eolid}}
#' 
#' @export
#' @author Scott Chamberlain, \email{myrmecocystus@@gmail.com}
#' 
#' @examples \dontrun{
#' get_colid(sciname='Poa annua')
#' get_colid(sciname='Pinus contorta')
#' get_colid(sciname='Puma concolor')
#' 
#' get_colid(c("Poa annua", "Pinus contorta"))
#' 
#' # When not found
#' get_colid(sciname="uaudnadndj")
#' get_colid(c("Chironomus riparius", "uaudnadndj"))
#' }

get_colid <- function(sciname, ask = TRUE, verbose = TRUE){
  fun <- function(sciname, ask, verbose) {
    mssg(verbose, "\nRetrieving data for taxon '", sciname, "'\n")
    df <- col_search(name=sciname)[[1]]
    
    if(nrow(df)==0){
      mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
      id <- NA
    } else
    {
      df <- df[,c('id','name','rank','status','acc_name')]
      names(df)[1] <- 'colid'
      id <- df$colid
    }
    
    # not found on eol
    if(length(id) == 0){
      mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
      id <- NA
    }
    # more than one found -> user input
    if(length(id) > 1){
      if(ask){
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
          message("Input accepted, took eolid '", as.character(df$colid[take]), "'.\n")
          id <- as.character(df$colid[take])
        } else {
          id <- NA
          mssg(verbose, "\nReturned 'NA'!\n\n")
        }
        } else{
          id <- NA
        }
    }  
    return(id)
  }
  sciname <- as.character(sciname)
  out <- sapply(sciname, fun, ask=ask, verbose=verbose, USE.NAMES = FALSE)
  class(out) <- "colid"
  if(!is.na(out[1])){
    urlmake <- na.omit(out)
    attr(out, 'uri') <- 
      sprintf('http://www.catalogueoflife.org/col/details/species/id/%s', urlmake)
  }
  return(out)
}