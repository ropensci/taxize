#' Get the BOLD (Barcode of Life) code for a search term.
#'
#' @importFrom bold bold_tax_name bold_tax_id
#' @export
#' @param searchterm character; A vector of common or scientific names.
#' @param fuzzy (logical) Whether to use fuzzy search or not (default: FALSE).
#' @param dataTypes (character) Specifies the datatypes that will be returned.
#' See \code{\link{bold_search}} for options.
#' @param includeTree (logical) If TRUE (default: FALSE), returns a list
#' containing information for parent taxa as well as the specified taxon.
#' @param ask logical; should get_tsn be run in interactive mode?
#' If TRUE and more than one TSN is found for teh species, the user is asked for
#' input. If FALSE NA is returned for multiple matches.
#' @param verbose logical; should progress be printed?
#' @param x Input to \code{\link{as.boldid}}
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @param rows numeric; Any number from 1 to infinity. If the default NA, all rows are
#' considered. Note that this function still only gives back a boldid class object with one
#' to many identifiers. See \code{\link[taxize]{get_boldid_}} to get back all, or a subset,
#' of the raw data that you are presented during the ask process.
#' @param division (character) A division (aka phylum) name. Optional. See \code{Filtering}
#' below.
#' @param parent (character) A parent name (i.e., the parent of the target search
#' taxon). Optional. See \code{Filtering} below.
#' @param rank (character) A taxonomic rank name. See \code{\link{rank_ref}} for possible
#' options. Though note that some data sources use atypical ranks, so inspect the
#' data itself for options. Optional. See \code{Filtering} below.
#' @param check logical; Check if ID matches any existing on the DB, only used in
#' \code{\link{as.boldid}}
#' @template getreturn
#'
#' @section Filtering:
#' The parameters \code{division}, \code{parent}, and \code{rank} are not
#' used in the search to the data provider, but are used in filtering the data down to a
#' subset that is closer to the target you want.  For all these parameters,
#' you can use regex strings since we use \code{\link{grep}} internally to match.
#' Filtering narrows down to the set that matches your query, and removes the rest.
#'
#' @family taxonomic-ids
#' @seealso \code{\link[taxize]{classification}}
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
#' # Narrow down results to a division or rank, or both
#' ## Satyrium example
#' ### Results w/o narrowing
#' get_boldid("Satyrium")
#' ### w/ phylum
#' get_boldid("Satyrium", division = "Plants")
#' get_boldid("Satyrium", division = "Animals")
#'
#' ## Rank example
#' get_boldid("Osmia", fuzzy = TRUE)
#' get_boldid("Osmia", fuzzy = TRUE, rank = "genus")
#'
#' # Fuzzy filter on any filtering fields
#' ## uses grep on the inside
#' get_boldid("Satyrium", division = "anim")
#' get_boldid("Aga", fuzzy = TRUE, parent = "*idae")
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
#' }

get_boldid <- function(searchterm, fuzzy = FALSE, dataTypes = 'basic',
                       includeTree = FALSE, ask = TRUE, verbose = TRUE,
                       rows = NA, rank = NULL, division = NULL,
                       parent = NULL, ...) {

  assert(verbose, "logical")
  assert(fuzzy, "logical")
  assert(dataTypes, "character")
  assert(includeTree, "logical")
  assert(rank, "character")
  assert(division, "character")
  assert(parent, "character")

  fun <- function(x, ask, verbose, rows) {
    direct <- FALSE
    mssg(verbose, "\nRetrieving data for taxon '", x, "'\n")
    bold_df <- bold_search(name = x, fuzzy = fuzzy,
                           dataTypes = dataTypes, includeTree = includeTree, ...)
    mm <- NROW(bold_df) > 1
    #bold_df <- sub_rows(bold_df, rows)

    if (!class(bold_df) == "data.frame") {
      boldid <- NA_character_
      att <- "not found"
    } else {
      if (all(names(bold_df) == "input")) {
        boldid <- NA_character_
        att <- "not found"
      } else {
        bold_df <- rename(bold_df, c('parentname' = 'parent', 'tax_rank' = 'rank',
                                     'tax_division' = 'division'))

        bold_df <- bold_df[,c("taxid","taxon","rank","division","parentid","parent")]

        # should return NA if spec not found
        if (nrow(bold_df) == 0) {
          mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
          boldid <- NA_character_
          att <- 'not found'
        }
        # take the one tsn from data.frame
        if (nrow(bold_df) == 1) {
          boldid <- bold_df$taxid
          att <- 'found'
        }
        # check for direct match
        if (nrow(bold_df) > 1) {
          names(bold_df)[grep('taxon', names(bold_df))] <- "target"
          di_rect <- match(tolower(bold_df$target), tolower(x))
          if (length(di_rect) == 1) {
            if (!all(is.na(di_rect))) {
              boldid <- bold_df$taxid[!is.na(di_rect)]
              direct <- TRUE
              att <- 'found'
            } else {
              boldid <- NA_character_
              att <- 'not found'
            }
          } else {
            boldid <- NA_character_
            att <- 'found'
          }
        }
        # multiple matches
        if (any(
          nrow(bold_df) > 1 & is.na(boldid) |
          nrow(bold_df) > 1 & att == "found" & length(boldid) > 1
        )) {
          assert(ask, "logical")
          if (ask) {
            names(bold_df)[grep('^taxon$', names(bold_df))] <- "target"

            if (!is.null(division) || !is.null(parent) || !is.null(rank)) {
              bold_df <- filt(bold_df, "division", division)
              bold_df <- filt(bold_df, "parent", parent)
              bold_df <- filt(bold_df, "rank", rank)
            }

            bold_df <- sub_rows(bold_df, rows)
            boldid <- id <- bold_df$taxid
            if (length(id) == 1) {
              direct <- TRUE
              att <- "found"
            }

            # user prompt
            bold_df <- bold_df[order(bold_df$target), ]
            rownames(bold_df) <- 1:nrow(bold_df)
            if (length(boldid) > 1 || NROW(bold_df) > 1) {
              # prompt
              message("\n\n")
              print(bold_df)
              message("\nMore than one TSN found for taxon '", x, "'!\n
            Enter rownumber of taxon (other inputs will return 'NA'):\n") # prompt
              take <- scan(n = 1, quiet = TRUE, what = 'raw')

              if (length(take) == 0)
                take <- 'notake'
              if (take %in% seq_len(nrow(bold_df))) {
                take <- as.numeric(take)
                message("Input accepted, took taxon '", as.character(bold_df$target[take]), "'.\n")
                boldid <-  bold_df$taxid[take]
                att <- 'found'
              } else {
                boldid <- NA_character_
                mssg(verbose, "\nReturned 'NA'!\n\n")
                att <- 'not found'
              }
            }
          } else {
            boldid <- NA_character_
            att <- "NA due to ask=FALSE"
          }
        }
      }
    }
    data.frame(
      boldid = as.character(boldid),
      att = att,
      multiple = mm,
      direct = direct,
      stringsAsFactors = FALSE)
  }
  searchterm <- as.character(searchterm)
  outd <- ldply(searchterm, fun, ask, verbose, rows)
  out <- structure(outd$boldid, class = "boldid",
                   match = outd$att,
                   multiple_matches = outd$multiple,
                   pattern_match = outd$direct)
  add_uri(out, 'http://boldsystems.org/index.php/Taxbrowser_Taxonpage?taxid=%s')
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
as.boldid.data.frame <- function(x, check=TRUE) {
  structure(x$ids, class="boldid", match=x$match,
            multiple_matches = x$multiple_matches,
            pattern_match = x$pattern_match, uri=x$uri)
}

#' @export
#' @rdname get_boldid
as.data.frame.boldid <- function(x, ...){
  data.frame(ids = as.character(unclass(x)),
             class = "boldid",
             match = attr(x, "match"),
             multiple_matches = attr(x, "multiple_matches"),
             pattern_match = attr(x, "pattern_match"),
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
