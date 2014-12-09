#' Get the BOLD (Barcode of Life) code for a search term.
#'
#' @import plyr
#' @importFrom bold bold_tax_name bold_tax_id
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
#' @param x Input to \code{\link{as.boldid}}
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @param rows numeric; Any number from 1 to inifity. If the default NA, all rows are considered.
#' Note that this function still only gives back a boldid class object with one to many identifiers.
#' See \code{\link[taxize]{get_boldid_}} to get back all, or a subset, of the raw data that you are
#' presented during the ask process.
#' @param check logical; Check if ID matches any existing on the DB, only used in
#' \code{\link{as.boldid}}
#'
#' @return A vector of BOLD ids. If a taxon is not found NA. If more than one BOLD ID is found
#'    the function asks for user input (if ask = TRUE), otherwise returns NA.
#'    Comes with an attribute \emph{match} to investigate the reason for NA (either 'not found',
#'    'found' or if ask = FALSE 'multi match')
#'
#' @seealso \code{\link[taxize]{get_uid}}, \code{\link[taxize]{classification}}
#'
#' @examples \dontrun{
#' get_boldid(searchterm = "Agapostemon")
#' get_boldid(searchterm = "Chironomus riparius")
#' get_boldid(c("Chironomus riparius","Quercus douglasii"))
#' splist <- names_list('species')
#' get_boldid(splist, verbose=FALSE)
#'
#' # Fuzzy searching
#' get_boldid(searchterm="Osmi", fuzzy=TRUE)
#'
#' # Get back a subset
#' get_boldid(searchterm="Osmi", fuzzy=TRUE, rows = 1)
#' get_boldid(searchterm="Osmi", fuzzy=TRUE, rows = 1:10)
#' get_boldid(searchterm=c("Osmi","Aga"), fuzzy=TRUE, rows = 1)
#' get_boldid(searchterm=c("Osmi","Aga"), fuzzy=TRUE, rows = 1:3)
#'
#' # When not found
#' get_boldid("howdy")
#' get_boldid(c("Chironomus riparius", "howdy"))
#' get_boldid('Epicordulia princeps')
#' get_boldid('Arigomphus furcifer')
#' get_boldid("Cordulegaster erronea")
#' get_boldid("Nasiaeshna pentacantha")
#'
#' # Convert a boldid without class information to a boldid class
#' as.boldid(get_boldid("Agapostemon")) # already a boldid, returns the same
#' as.boldid(get_boldid(c("Agapostemon","Quercus douglasii"))) # same
#' as.boldid(1973) # numeric
#' as.boldid(c(1973,101009,98597)) # numeric vector, length > 1
#' as.boldid("1973") # character
#' as.boldid(c("1973","101009","98597")) # character vector, length > 1
#' as.boldid(list("1973","101009","98597")) # list, either numeric or character
#' ## dont check, much faster
#' as.boldid("1973", check=FALSE)
#' as.boldid(1973, check=FALSE)
#' as.boldid(c("1973","101009","98597"), check=FALSE)
#' as.boldid(list("1973","101009","98597"), check=FALSE)
#'
#' (out <- as.boldid(c(1973,101009,98597)))
#' data.frame(out)
#' as.boldid( data.frame(out) )
#'
#' # Get all data back
#' get_boldid_("Osmia", fuzzy=TRUE, rows=1:5)
#' get_boldid_("Osmia", fuzzy=TRUE, rows=1)
#' get_boldid_(c("Osmi","Aga"), fuzzy=TRUE, rows = 1:3)
#'
#' # Curl options
#' library("httr")
#' get_boldid(searchterm = "Agapostemon", config=verbose())
#' get_boldid(searchterm = "Agapostemon", config=progress())
#' }

get_boldid <- function(searchterm, fuzzy = FALSE, dataTypes='basic', includeTree=FALSE,
                       ask = TRUE, verbose = TRUE, rows = NA, ...)
{
  fun <- function(x, ask, verbose, rows)
  {
    mssg(verbose, "\nRetrieving data for taxon '", x, "'\n")
    bold_df <- bold_search(name = x, fuzzy = fuzzy,
                           dataTypes = dataTypes, includeTree = includeTree, ...)
    bold_df <- sub_rows(bold_df, rows)

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
  outd <- ldply(searchterm, fun, ask, verbose, rows)
  out <- structure(outd$boldid, class="boldid", match=outd$att)
  add_uri(out, 'http://boldsystems.org/index.php/Taxbrowser_Taxonpage?taxid=%s')
#   if( !all(is.na(out)) ){
#     urlmake <- na.omit(out)
#     attr(out, 'uri') <-
#       sprintf('http://boldsystems.org/index.php/Taxbrowser_Taxonpage?taxid=%s', urlmake)
#   }
#   class(out) <- "boldid"
#   return(out)
}

#' @export
#' @rdname get_boldid
as.boldid <- function(x, check=TRUE) UseMethod("as.boldid")

#' @export
#' @rdname get_boldid
as.boldid.boldid <- function(x, check=TRUE) x

#' @export
#' @rdname get_boldid
as.boldid.character <- function(x, check=TRUE) if(length(x) == 1) make_boldid(x, check) else collapse(x, make_boldid, "boldid", check=check)

#' @export
#' @rdname get_boldid
as.boldid.list <- function(x, check=TRUE) if(length(x) == 1) make_boldid(x, check) else collapse(x, make_boldid, "boldid", check=check)

#' @export
#' @rdname get_boldid
as.boldid.numeric <- function(x, check=TRUE) as.boldid(as.character(x), check)

#' @export
#' @rdname get_boldid
as.boldid.data.frame <- function(x, check=TRUE) structure(x$ids, class="boldid", match=x$match, uri=x$uri)

#' @export
#' @rdname get_boldid
as.data.frame.boldid <- function(x, ...){
  data.frame(ids = as.character(unclass(x)),
             class = "boldid",
             match = attr(x, "match"),
             uri = attr(x, "uri"),
             stringsAsFactors = FALSE)
}

make_boldid <- function(x, check=TRUE) make_generic(x, 'http://boldsystems.org/index.php/Taxbrowser_Taxonpage?taxid=%s', "boldid", check)

check_boldid <- function(x){
  tryid <- bold_tax_id(x)
  !identical("noresults", names(tryid)[2])
}

#' @export
#' @rdname get_boldid
get_boldid_ <- function(searchterm, verbose = TRUE, fuzzy = FALSE, dataTypes='basic', includeTree=FALSE, rows = NA, ...){
  setNames(lapply(searchterm, get_boldid_help, verbose = verbose, fuzzy = fuzzy, dataTypes=dataTypes, includeTree=includeTree, rows = rows, ...), searchterm)
}

get_boldid_help <- function(searchterm, verbose, fuzzy, dataTypes, includeTree, rows, ...){
  mssg(verbose, "\nRetrieving data for taxon '", searchterm, "'\n")
  df <- bold_search(name = searchterm, fuzzy = fuzzy, dataTypes = dataTypes, includeTree = includeTree)
  if(NROW(df) == 0) NULL else sub_rows(df, rows)
}
