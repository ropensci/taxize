#' Get the uBio id for a search term.
#'
#' Retrieve the uBio id of a taxon. This function uses \code{\link[taxize]{ubio_search}} internally
#' to search for names.
#'
#' @import plyr
#' @param searchterm character; A vector of common or scientific names.
#' @param searchtype character; One of 'scientific' or 'common', or any unique abbreviation
#' @param ask logical; should get_tsn be run in interactive mode?
#' If TRUE and more than one TSN is found for teh species, the user is asked for
#' input. If FALSE NA is returned for multiple matches.
#' @param verbose logical; should progress be printed?
#'
#' @return A vector of uBio ids. If a taxon is not found NA is given. If more than one uBio
#'    id is found the function asks for user input (if ask = TRUE), otherwise returns NA.
#'    Comes with an attribute \emph{match} to investigate the reason for NA (either 'not found',
#'    'found' or if ask = FALSE 'multi match')
#'
#' @seealso \code{\link[taxize]{get_uid}}, \code{\link[taxize]{ubio_search}}
#'
#' @export
#' @examples \dontrun{
#' get_ubioid(searchterm = "Astragalus aduncus")
#' get_ubioid(c("Salvelinus fontinalis","Pomacentrus brachialis"))
#' splist <- c("Salvelinus fontinalis", 'Pomacentrus brachialis', "Leptocottus armatus",
#' 		"Clinocottus recalvus", "Trachurus trachurus", "Harengula clupeola")
#' get_ubioid(splist, verbose=FALSE)
#'
#' # When not found
#' get_ubioid(searchterm="howdy")
#' get_ubioid(c("Salvelinus fontinalis", "howdy"))
#'
#' # Using common names
#' get_ubioid(searchterm="great white shark", searchtype="common")
#' get_ubioid(searchterm=c("bull shark", "whale shark"), searchtype="common")
#' }

get_ubioid <- function(searchterm, searchtype = "scientific", ask = TRUE, verbose = TRUE)
{
  fun <- function(x, searchtype, ask, verbose)
  {
    mssg(verbose, "\nRetrieving data for taxon '", x, "'\n")

    searchtype <- match.arg(searchtype, c("scientific","common"))
    if(searchtype=='scientific'){ sci <- 1; vern <- 0 } else { sci <- 0; vern <- 1; searchtype='vernacular' }
    ubio_df <-  tryCatch(ubio_search(searchName = x, sci = sci, vern = vern)[[searchtype]], error=function(e) e)

    if(is(ubio_df, "simpleError")){
      ubioid <- NA
      att <- "not found"
    } else {
      ubio_df <- switch(searchtype,
                        scientific=ubio_df[,c("namebankid","namestring","packagename","rankname")],
                        vernacular=ubio_df[,c("namebankid","namestring","packagename")])
      ubio_df <- rename(ubio_df, c('packagename' = 'family'))

      direct <- NA
      # should return NA if spec not found
      if (nrow(ubio_df) == 0){
        mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
        ubioid <- NA
        att <- 'not found'
      }
      # take the one ubio id from data.frame
      if (nrow(ubio_df) == 1){
        ubioid <- ubio_df$namebankid
        att <- 'found'
      }
      # check for direct match
      if (nrow(ubio_df) > 1){
        names(ubio_df)[grep('namestring', names(ubio_df))] <- "target"
        direct <- match(tolower(ubio_df$target), tolower(x))
        if(!all(is.na(direct))){
          ubioid <- ubio_df$namebankid[!is.na(direct)]
          att <- 'found'
        } else {
          ubioid <- NA
          direct <- NA
          att <- 'not found'
        }
      }
      # multiple matches
      if( any(
        nrow(ubio_df) > 1 & is.na(ubioid) |
          nrow(ubio_df) > 1 & att == "found" & length(ubioid) > 1
      ) ){
        if(ask) {
          names(ubio_df)[names(ubio_df) %in% "namestring"] <- "target"
          # user prompt
          ubio_df <- ubio_df[order(ubio_df$target), ]
          rownames(ubio_df) <- 1:nrow(ubio_df)

          # prompt
          message("\n\n")
          print(ubio_df)
          message("\nMore than one uBio ID found for taxon '", x, "'!\n
            Enter rownumber of taxon (other inputs will return 'NA'):\n") # prompt
          take <- scan(n = 1, quiet = TRUE, what = 'raw')

          if(length(take) == 0)
            take <- 'notake'
          if(take %in% seq_len(nrow(ubio_df))){
            take <- as.numeric(take)
            message("Input accepted, took taxon '", as.character(ubio_df$target[take]), "'.\n")
            ubioid <-  ubio_df$namebankid[take]
            att <- 'found'
          } else {
            ubioid <- NA
            mssg(verbose, "\nReturned 'NA'!\n\n")
            att <- 'not found'
          }
        } else {
          ubioid <- NA
          att <- 'multi match'
        }
      }

    }
    return(data.frame(ubioid = as.character(ubioid), att = att, stringsAsFactors=FALSE))
  }
  searchterm <- as.character(searchterm)
  outd <- ldply(searchterm, fun, searchtype, ask, verbose)
  out <- outd$ubioid
  attr(out, 'match') <- outd$att
  if( !all(is.na(out)) ){
    urlmake <- na.omit(out)
    attr(out, 'uri') <-
      sprintf('http://www.ubio.org/browser/details.php?namebankID=%s', urlmake)
  }
  class(out) <- "ubioid"
  return(out)
}
