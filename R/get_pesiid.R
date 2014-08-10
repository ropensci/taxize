#' Get the PESI id for a search term.
#'
#' Retrieve the PESI id of a taxon.
#'
#' @import plyr
#' @param searchterm character; A vector of common or scientific names.
#' @param searchtype character; One of 'scientific' or 'common', or any unique abbreviation
#' @param accepted logical; If TRUE (default), removes names that are not accepted valid names
#' by PESI. Set to FALSE to give back both accepted and unaccepted names.
#' @param ask logical; should get_tsn be run in interactive mode?
#' If TRUE and more than one TSN is found for teh species, the user is asked for
#' input. If FALSE NA is returned for multiple matches.
#' @param verbose logical; should progress be printed?
#'
#' @return A vector of PESI ids. If a taxon is not found NA is given. If more than one PESI
#'    id is found the function asks for user input (if ask = TRUE), otherwise returns NA.
#'    Comes with an attribute \emph{match} to investigate the reason for NA (either 'not found',
#'    'found' or if ask = FALSE 'multi match')
#'
#' @seealso \code{\link[taxize]{get_uid}}, \code{\link[taxize]{classification}}
#'
#' @export
#' @examples \dontrun{
#' get_pesiid(searchterm = "Salvelinus")
#' get_pesiid(c("Salvelinus fontinalis","Pomacentrus brachialis"))
#' splist <- c("Salvelinus fontinalis", 'Pomacentrus brachialis', "Leptocottus armatus",
#'   	"Clinocottus recalvus", "Trachurus trachurus", "Harengula clupeola")
#' get_pesiid(splist, verbose=FALSE)
#'
#' # When not found
#' get_pesiid(searchterm="howdy")
#' get_pesiid(c("Salvelinus fontinalis", "howdy"))
#'
#' # Using common names
#' get_pesiid(searchterm="salmon", searchtype="common")
#' get_pesiid(searchterm="great white shark", searchtype="common")
#' }

get_pesiid <- function(searchterm, searchtype = "scientific", accepted = TRUE, ask = TRUE, verbose = TRUE)
{
  fun <- function(x, searchtype, ask, verbose)
  {
    mssg(verbose, "\nRetrieving data for taxon '", x, "'\n")

    searchtype <- match.arg(searchtype, c("scientific","common"))
    pesi_df <-  suppressWarnings(switch(searchtype,
                                         scientific=pesi_records(scientific = x),
                                         common=pesi_records(common = x)))

    if(is.null(pesi_df)){
      pesiid <- NA
      att <- "not found"
    } else {
      pesi_df <- pesi_df[,c("scientificname","status","rank","GUID")]

      direct <- NA
      # should return NA if spec not found
      if (nrow(pesi_df) == 0){
        mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
        pesiid <- NA
        att <- 'not found'
      }
      # take the one worms id from data.frame
      if (nrow(pesi_df) == 1){
        pesiid <- pesi_df$GUID
        att <- 'found'
      }
      # check for direct match
      if (nrow(pesi_df) > 1){
        names(pesi_df)[grep(searchtype, names(pesi_df))] <- "target"
        direct <- match(tolower(pesi_df$target), tolower(x))
        if(!all(is.na(direct))){
          pesiid <- pesi_df$GUID[!is.na(direct)]
          att <- 'found'
        } else {
          pesiid <- NA
          direct <- NA
          att <- 'not found'
        }
      }
      # multiple matches
      if( any(
        nrow(pesi_df) > 1 & is.na(pesiid) |
          nrow(pesi_df) > 1 & att == "found" & length(pesiid) > 1
      ) ){
        if(ask) {
          names(pesi_df)[names(pesi_df) %in% "scientificname"] <- "target"
          # user prompt
          pesi_df <- pesi_df[order(pesi_df$target), ]
          rownames(pesi_df) <- 1:nrow(pesi_df)

          # prompt
          message("\n\n")
          print(pesi_df)
          message("\nMore than one PESI ID found for taxon '", x, "'!\n
                  Enter rownumber of taxon (other inputs will return 'NA'):\n") # prompt
          take <- scan(n = 1, quiet = TRUE, what = 'raw')

          if(length(take) == 0)
            take <- 'notake'
          if(take %in% seq_len(nrow(pesi_df))){
            take <- as.numeric(take)
            message("Input accepted, took taxon '", as.character(pesi_df$target[take]), "'.\n")
            pesiid <-  pesi_df$GUID[take]
            att <- 'found'
          } else {
            pesiid <- NA
            mssg(verbose, "\nReturned 'NA'!\n\n")
            att <- 'not found'
          }
        } else {
          pesiid <- NA
          att <- 'multi match'
        }
      }

    }
    return(data.frame(pesiid = as.character(pesiid), att = att, stringsAsFactors=FALSE))
  }
  searchterm <- as.character(searchterm)
  outd <- ldply(searchterm, fun, searchtype, ask, verbose)
  out <- outd$pesiid
  attr(out, 'match') <- outd$att
  if( !all(is.na(out)) ){
    urlmake <- na.omit(out)
    attr(out, 'uri') <-
      sprintf('http://www.eu-nomen.eu/portal/taxon.php?GUID=%s', urlmake)
  }
  class(out) <- "pesiid"
  return(out)
}
