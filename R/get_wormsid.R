#' Get the WoRMS code for a search term.
#'
#' Retrieve the WoRMS id of a taxon.
#'
#' @import plyr
#' @param searchterm character; A vector of common or scientific names.
#' @param searchtype character; One of 'scientific' or 'common', or any unique abbreviation
#' @param accepted logical; If TRUE (default), removes names that are not accepted valid names
#' by WoRMS. Set to FALSE to give back both accepted and unaccepted names.
#' @param ask logical; should get_tsn be run in interactive mode?
#' If TRUE and more than one TSN is found for teh species, the user is asked for
#' input. If FALSE NA is returned for multiple matches.
#' @param verbose logical; should progress be printed?
#'
#' @return A vector of WoRMS ids. If a taxon is not found NA is given. If more than one WoRMS
#'    id is found the function asks for user input (if ask = TRUE), otherwise returns NA.
#'    Comes with an attribute \emph{match} to investigate the reason for NA (either 'not found',
#'    'found' or if ask = FALSE 'multi match')
#'
#' @seealso \code{\link[taxize]{get_uid}}, \code{\link[taxize]{classification}}
#'
#' @export
#' @examples \dontrun{
#' get_wormsid(searchterm = "Salvelinus fontinalis")
#' get_wormsid(c("Salvelinus fontinalis","Pomacentrus brachialis"))
#' splist <- c("Salvelinus fontinalis", 'Pomacentrus brachialis', "Leptocottus armatus",
#' 		"Clinocottus recalvus", "Trachurus trachurus", "Harengula clupeola")
#' get_wormsid(splist, verbose=FALSE)
#'
#' # When not found
#' get_wormsid(searchterm="howdy")
#' get_wormsid(c("Salvelinus fontinalis", "howdy"))
#'
#' # Using common names
#' get_wormsid(searchterm="salmon", searchtype="common")
#' }

get_wormsid <- function(searchterm, searchtype = "scientific", accepted = TRUE, ask = TRUE, verbose = TRUE)
{
  fun <- function(x, searchtype, ask, verbose)
  {
    mssg(verbose, "\nRetrieving data for taxon '", x, "'\n")

    searchtype <- match.arg(searchtype, c("scientific","common"))
    worms_df <-  suppressWarnings(switch(searchtype,
                        scientific=worms_records(scientific = x),
                        common=worms_records(common = x)))

    if("noresults" %in% names(worms_df)){
      wormsid <- NA
      att <- "not found"
    } else {
      worms_df <- worms_df[,c("AphiaID","scientificname","status","rank","valid_AphiaID","valid_name")]

      if(accepted){
        worms_df <- worms_df[ worms_df$status %in% 'accepted', ]
      }

      direct <- NA
      # should return NA if spec not found
      if (nrow(worms_df) == 0){
        mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
        wormsid <- NA
        att <- 'not found'
      }
      # take the one worms id from data.frame
      if (nrow(worms_df) == 1){
        wormsid <- worms_df$AphiaID
        att <- 'found'
      }
      # check for direct match
      if (nrow(worms_df) > 1){
        names(worms_df)[grep(searchtype, names(worms_df))] <- "target"
        direct <- match(tolower(worms_df$target), tolower(x))
        if(!all(is.na(direct))){
          wormsid <- worms_df$AphiaID[!is.na(direct)]
          att <- 'found'
        } else {
          wormsid <- NA
          direct <- NA
          att <- 'not found'
        }
      }
      # multiple matches
      if( any(
        nrow(worms_df) > 1 & is.na(wormsid) |
          nrow(worms_df) > 1 & att == "found" & length(wormsid) > 1
      ) ){
        if(ask) {
          names(worms_df)[names(worms_df) %in% "scientificname"] <- "target"
          # user prompt
          worms_df <- worms_df[order(worms_df$target), ]
          rownames(worms_df) <- 1:nrow(worms_df)

          # prompt
          message("\n\n")
          print(worms_df)
          message("\nMore than one TSN found for taxon '", x, "'!\n
            Enter rownumber of taxon (other inputs will return 'NA'):\n") # prompt
          take <- scan(n = 1, quiet = TRUE, what = 'raw')

          if(length(take) == 0)
            take <- 'notake'
          if(take %in% seq_len(nrow(worms_df))){
            take <- as.numeric(take)
            message("Input accepted, took taxon '", as.character(worms_df$target[take]), "'.\n")
            wormsid <-  worms_df$AphiaID[take]
            att <- 'found'
          } else {
            wormsid <- NA
            mssg(verbose, "\nReturned 'NA'!\n\n")
            att <- 'not found'
          }
        } else {
          wormsid <- NA
          att <- 'multi match'
        }
      }

    }
    return(data.frame(wormsid = as.character(wormsid), att = att, stringsAsFactors=FALSE))
  }
  searchterm <- as.character(searchterm)
  outd <- ldply(searchterm, fun, searchtype, ask, verbose)
  out <- outd$wormsid
  attr(out, 'match') <- outd$att
  if( !all(is.na(out)) ){
    urlmake <- na.omit(out)
    attr(out, 'uri') <-
      sprintf('http://www.marinespecies.org/aphia.php?p=taxdetails&id=%s', urlmake)
  }
  class(out) <- "wormsid"
  return(out)
}
