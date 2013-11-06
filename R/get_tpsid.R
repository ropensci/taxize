#' Get the NameID codes from Tropicos for taxonomic names.
#' 
#' @import plyr RCurl
#' @param sciname character; scientific name.
#' @param verbose logical; If TRUE the actual taxon queried is printed on the 
#'    console.
#' 
#' @return A vector of unique identifiers. If a taxon is not found NA. 
#' If more than one ID is found the function asks for user input. 
#' 
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{get_uid}}
#' 
#' @export
#' @author Scott Chamberlain, \email{myrmecocystus@@gmail.com}
#' 
#' @examples \dontrun{
#' get_tpsid(sciname='Poa annua')
#' get_tpsid(sciname='Pinus contorta')
#' 
#' get_tpsid(c("Poa annua", "Pinus contorta"))
#' 
#' # When not found
#' get_tpsid("howdy")
#' get_tpsid(c("Chironomus riparius", "howdy"))
#' }
get_tpsid <- function(sciname, verbose = TRUE){
  fun <- function(sciname) {
    if(verbose)
      message("\nRetrieving data for taxon '", sciname, "'\n")
    df <- tp_search(name = sciname)[,c('NameId','ScientificName','RankAbbreviation','NomenclatureStatusName')]
    names(df) <- c('tpsid','name','rank','status')
    id <- df$tpsid
    
    # not found on tropicos
    if(length(id) == 0){
      message("Not found. Consider checking the spelling or alternate classification")
      id <- NA
    }
    # more than one found on tropicos -> user input
    if(length(id) > 1){
      rownames(df) <- 1:nrow(df)
      # prompt
      message("\n\n")
      message("\nMore than one tpsid found for taxon '", sciname, "'!\n
          Enter rownumber of taxon (other inputs will return 'NA'):\n")      
      print(df)
      take <- scan(n = 1, quiet = TRUE, what = 'raw')
      
      if(length(take) == 0)
        take <- 'notake'
      if(take %in% seq_len(nrow(df))){
        take <- as.numeric(take)
        message("Input accepted, took tpsid '", as.character(df$tpsid[take]), "'.\n")
        id <- as.character(df$tpsid[take])
      } else {
        id <- NA
        message("\nReturned 'NA'!\n\n")
      }
    }  
    return(id)
  }
  out <- laply(sciname, fun)
  class(out) <- "tpsid"
  return(out)
}