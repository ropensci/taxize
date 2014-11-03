#' Get the BOLD (Barcode of Life) code for a search term.
#'
#' @import plyr
#' @export
#' @param searchterm character; A vector of common or scientific names.
#' @param fuzzy (logical) Whether to use fuzzy search or not (default: FALSE).
#' @param dataTypes (character) Specifies the datatypes that will be returned. See Details for 
#' options.
#' @param includeTree (logical) If TRUE (default: FALSE), returns a list containing information
#' for parent taxa as well as the specified taxon.
#' @param ask logical; should get_tsn be run in interactive mode?
#' If TRUE and more than one TSN is found for teh species, the user is asked for
#' input. If FALSE NA is returned for multiple matches.
#' @param verbose logical; should progress be printed?
#'
#' @return A vector of BOLD ids. If a taxon is not found NA. If more than one BOLD ID is found
#'    the function asks for user input (if ask = TRUE), otherwise returns NA.
#'    Comes with an attribute \emph{match} to investigate the reason for NA (either 'not found',
#'    'found' or if ask = FALSE 'multi match')
#'
#' @seealso \code{\link[taxize]{get_uid}}, \code{\link[taxize]{classification}}
#'
#' @examples \donttest{
#' get_boldid(searchterm = "Agapostemon")
#' get_boldid(searchterm = "Chironomus riparius")
#' get_boldid(c("Chironomus riparius","Quercus douglasii")) # needs error catching
#' splist <- names_list('species')
#' get_boldid(splist, verbose=FALSE)
#' 
#' # Fuzzy searching
#' get_boldid(searchterm="Osmi", fuzzy=TRUE)
#' 
#' # When not found
#' get_boldid("howdy")
#' get_boldid(c("Chironomus riparius", "howdy"))
#' get_boldid('Epicordulia princeps')
#' get_boldid('Arigomphus furcifer')
#' get_boldid("Cordulegaster erronea")
#' get_boldid("Nasiaeshna pentacantha")
#' }

get_boldid <- function(searchterm, fuzzy = FALSE, dataTypes='basic', includeTree=FALSE,
                       ask = TRUE, verbose = TRUE)
{
  fun <- function(x, ask, verbose)
  {
    mssg(verbose, "\nRetrieving data for taxon '", x, "'\n")
    
    bold_df <- bold_search(name = x, fuzzy = fuzzy, dataTypes = dataTypes, includeTree = includeTree)
    
    
    if(!class(bold_df) == "data.frame"){
      boldid <- NA
      att <- "not found"
    } else {
      
      if(all(names(bold_df) == "input")){
        boldid <- NA
        att <- "not found"
      } else {
        
        bold_df <- bold_df[,c("taxid","taxon","tax_rank","tax_division","parentid","parentname")]
        
        direct <- NA
        # should return NA if spec not found
        if (nrow(bold_df) == 0){
          mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
          boldid <- NA
          att <- 'not found'
        }
        # take the one tsn from data.frame
        if (nrow(bold_df) == 1){
          boldid <- bold_df$taxid
          att <- 'found'
        }
        # check for direct match
        if (nrow(bold_df) > 1){
          names(bold_df)[grep('taxon', names(bold_df))] <- "target"
          #       direct <- match(tolower(x), tolower(bold_df$target))
          direct <- match(tolower(bold_df$target), tolower(x))
          if(!all(is.na(direct))){
            #         tsn <- bold_df$tsn[direct]
            boldid <- bold_df$taxid[!is.na(direct)]
            att <- 'found'
          } else {
            boldid <- NA
            direct <- NA
            att <- 'not found'
          }
        }
        # multiple matches
        if( any(
          nrow(bold_df) > 1 & is.na(boldid) |
            nrow(bold_df) > 1 & att == "found" & length(boldid) > 1
        ) ){
          if(ask) {
            names(bold_df)[grep('taxon', names(bold_df))] <- "target"
            # user prompt
            bold_df <- bold_df[order(bold_df$target), ]
            rownames(bold_df) <- 1:nrow(bold_df)
            
            # prompt
            message("\n\n")
            print(bold_df)
            message("\nMore than one TSN found for taxon '", x, "'!\n
            Enter rownumber of taxon (other inputs will return 'NA'):\n") # prompt
            take <- scan(n = 1, quiet = TRUE, what = 'raw')
            
            if(length(take) == 0)
              take <- 'notake'
            if(take %in% seq_len(nrow(bold_df))){
              take <- as.numeric(take)
              message("Input accepted, took taxon '", as.character(bold_df$target[take]), "'.\n")
              boldid <-  bold_df$taxid[take]
              att <- 'found'
            } else {
              boldid <- NA
              mssg(verbose, "\nReturned 'NA'!\n\n")
              att <- 'not found'
            }
          } else {
            boldid <- NA
            att <- 'multi match'
          }
        }
      }
    }
    return(data.frame(boldid = as.character(boldid), att = att, stringsAsFactors=FALSE))
  }
  searchterm <- as.character(searchterm)
  outd <- ldply(searchterm, fun, ask, verbose)
  out <- outd$boldid
  attr(out, 'match') <- outd$att
  if( !all(is.na(out)) ){
    urlmake <- na.omit(out)
    attr(out, 'uri') <-
      sprintf('http://boldsystems.org/index.php/Taxbrowser_Taxonpage?taxid=%s', urlmake)
  }
  class(out) <- "boldid"
  return(out)
}
