#' Get Kew's Plants of the World code for a taxon
#'
#' @export
#' @param x character; A vector of common or scientific names
#' @param accepted logical; If TRUE, removes names that are not accepted 
#' valid names by ITIS. Set to \code{FALSE} (default) to give back both 
#' accepted and unaccepted names.
#' @param ask logical; should \code{get_pow} be run in interactive mode?
#' If TRUE and more than one pow is found for teh species, the user is 
#' asked for input. If FALSE NA is returned for multiple matches.
#' @param messages logical; should progress be printed?
#' @param ... Curl options passed on to \code{\link[crul]{HttpClient}}
#' @param rows numeric; Any number from 1 to infinity. If the default NA, 
#' all rows are considered. Note that this function still only gives back 
#' a pow class object with one to many identifiers. See 
#' \code{\link[taxize]{get_pow_}} to get back all, or a subset,
#' of the raw data that you are presented during the ask process.
#' @param family_filter (character) A division (aka phylum) name to filter
#' data after retrieved from NCBI. Optional. See \code{Filtering} below.
#' @param rank_filter (character) A taxonomic rank name to filter data after
#' retrieved from NCBI. See \code{\link{rank_ref}} for possible options.
#' Though note that some data sources use atypical ranks, so inspect the data
#' itself for options. Optional. See \code{Filtering} below.
#' @param check logical; Check if ID matches any existing on the DB, only 
#' used in \code{\link{as.pow}}
#' @template getreturn
#' 
#' @family pow
#' 
#' @section Filtering:
#' The parameters \code{family_filter} and \code{rank_filter} are not
#' used in the search to the data provider, but are used in filtering the data down to a
#' subset that is closer to the target you want.  For these two parameters,
#' you can use regex strings since we use \code{\link{grep}} internally to match.
#' Filtering narrows down to the set that matches your query, and removes the rest.
#'
#' @family taxonomic-ids
#' @seealso \code{\link[taxize]{classification}}
#'
#' @examples \dontrun{
#' get_pow(x = "Helianthus")
#' get_pow(c("Helianthus","Quercus douglasii"))
#'
#' # Get back a subset
#' get_pow(x="Helianthus", rows = 1)
#' get_pow(x="Helianthus", rows = 1:10)
#'
#' # When not found
#' get_pow("howdy")
#' get_pow(c("Helianthus annuus", "howdy"))
#'
#' # Narrow down results 
#' # to accepted names
#' get_pow("Helianthus", accepted = TRUE)
#' # to a kingom 
#' get_pow("Helianthus", rank_filter = "genus")
#' # to accepted names and rank
#' get_pow("Helianthus annuus", accepted = TRUE, rank_filter = "species")
#' # to a family
#' get_pow("flower", family_filter = "Acanthaceae")
#'
#' # Convert a pow without class information to a pow class
#' z <- get_pow("Helianthus annuus", accepted = TRUE, rank_filter = "species")
#' # already a pow, returns the same
#' as.pow(z)
#' as.pow("urn:lsid:ipni.org:names:119003-2")
#' # character vector, length > 1
#' ids <- c("urn:lsid:ipni.org:names:119003-2","urn:lsid:ipni.org:names:328247-2")
#' as.pow(ids)
#' # list, with character strings
#' as.pow(as.list(ids)) 
#' ## dont check, much faster
#' as.pow("urn:lsid:ipni.org:names:119003-2", check=FALSE)
#' as.pow(ids, check=FALSE)
#' as.pow(as.list(ids), check=FALSE)
#'
#' (out <- as.pow(ids))
#' data.frame(out)
#' as.pow( data.frame(out) )
#'
#' # Get all data back
#' get_pow_("Quercus", rows=1:5)
#' get_pow_("Quercus", rows=1)
#' get_pow_(c("Pinus", "Abies"), rows = 1:3)
#' }

get_pow <- function(x, accepted = FALSE, ask = TRUE, messages = TRUE,
  rows = NA, family_filter = NULL, rank_filter = NULL, ...) {

  assert(accepted, "logical")
  assert(ask, "logical")
  assert(messages, "logical")
  assert(family_filter, "character")
  assert(rank_filter, "character")
  assert_rows(rows)

  fun <- function(x, ask, messages, rows) {
    direct <- FALSE
    mssg(messages, "\nRetrieving data for taxon '", x, "'\n")
    pow_df <- pow_search(q = x, ...)$data
    mm <- NROW(pow_df) > 1

    if (!inherits(pow_df, "data.frame")) {
      pow <- NA_character_
      att <- "not found"
    } else {
      pow_df <- pow_df[, c("name","rank","accepted","kingdom","family","fqId")]

      # should return NA if spec not found
      if (nrow(pow_df) == 0) {
        mssg(messages, "Not found. Consider checking the spelling or alternate classification")
        pow <- NA_character_
        att <- 'not found'
      }

      if (accepted) {
        pow_df <- pow_df[pow_df$accepted, ]
      }

      # take the one pow from data.frame
      if (nrow(pow_df) == 1) {
        pow <- pow_df$fqId
        att <- 'found'
      }
      # check for direct match
      if (nrow(pow_df) > 1) {
        names(pow_df)[grep('name', names(pow_df))] <- "target"
        di_rect <- match(tolower(pow_df$target), tolower(x))
        if (length(di_rect) == 1) {
          if (!all(is.na(di_rect))) {
            pow <- pow_df$fqId[!is.na(di_rect)]
            direct <- TRUE
            att <- 'found'
          } else {
            pow <- NA_character_
            att <- 'not found'
          }
        } else {
          direct <- FALSE
          pow <- NA_character_
          att <- 'found'
        }
      }
      # multiple matches
      if (any(
        nrow(pow_df) > 1 && is.na(pow) |
        nrow(pow_df) > 1 && att == "found" && length(pow) > 1
      )) {
        names(pow_df)[grep('^name$', names(pow_df))] <- "target"

        if (!is.null(family_filter) || !is.null(rank_filter)) {
          pow_df <- filt(pow_df, "family", family_filter)
          pow_df <- filt(pow_df, "rank", rank_filter)
        }

        pow_df <- sub_rows(pow_df, rows)
        pow <- id <- pow_df$fqId
        if (length(id) == 1) {
          direct <- TRUE
          att <- "found"
        }

        if (ask) {
          # user prompt
          pow_df <- pow_df[order(pow_df$target), ]
          rownames(pow_df) <- 1:nrow(pow_df)
          if (length(pow) > 1 || NROW(pow_df) > 1) {
            # prompt
            message("\n\n")
            print(pow_df)
            message("\nMore than one pow found for taxon '", x, "'!\n
          Enter rownumber of taxon (other inputs will return 'NA'):\n") # prompt
            take <- scan(n = 1, quiet = TRUE, what = 'raw')

            if (length(take) == 0)
              take <- 'notake'
            if (take %in% seq_len(nrow(pow_df))) {
              take <- as.numeric(take)
              message("Input accepted, took taxon '", as.character(pow_df$target[take]), "'.\n")
              pow <-  pow_df$fqId[take]
              att <- 'found'
            } else {
              pow <- NA_character_
              mssg(messages, "\nReturned 'NA'!\n\n")
              att <- 'not found'
            }
          }
        } else {
          if (length(pow) == 1) {
            att <- "found"
          } else {
            warning(
              sprintf("More than one pow found for taxon '%s'; refine query or set ask=TRUE",
                      x),
              call. = FALSE
            )
            pow <- NA_character_
            att <- 'NA due to ask=FALSE & > 1 result'
          }
        }
      }
    }
    data.frame(
      pow = as.character(pow),
      att = att,
      multiple = mm,
      direct = direct,
      stringsAsFactors = FALSE)
  }
  x <- as.character(x)
  outd <- ldply(x, fun, ask, messages, rows)
  out <- structure(outd$pow, class = "pow",
                   match = outd$att,
                   multiple_matches = outd$multiple,
                   pattern_match = outd$direct)
  add_uri(out, 'http://powo.science.kew.org/taxon/%s')
}

#' @export
#' @rdname get_pow
as.pow <- function(x, check=TRUE) UseMethod("as.pow")

#' @export
#' @rdname get_pow
as.pow.pow <- function(x, check=TRUE) x

#' @export
#' @rdname get_pow
as.pow.character <- function(x, check=TRUE) if(length(x) == 1) make_pow(x, check) else collapse(x, make_pow, "pow", check=check)

#' @export
#' @rdname get_pow
as.pow.list <- function(x, check=TRUE) if(length(x) == 1) make_pow(x, check) else collapse(x, make_pow, "pow", check=check)

#' @export
#' @rdname get_pow
as.pow.data.frame <- function(x, check=TRUE) {
  structure(x$ids, class="pow", match=x$match,
            multiple_matches = x$multiple_matches,
            pattern_match = x$pattern_match, uri=x$uri)
}

#' @export
#' @rdname get_pow
as.data.frame.pow <- function(x, ...){
  data.frame(ids = as.character(unclass(x)),
             class = "pow",
             match = attr(x, "match"),
             multiple_matches = attr(x, "multiple_matches"),
             pattern_match = attr(x, "pattern_match"),
             uri = attr(x, "uri"),
             stringsAsFactors = FALSE)
}

make_pow <- function(x, check=TRUE) make_generic(x, 'http://powo.science.kew.org/taxon/%s', "pow", check)

check_pow <- function(x){
  tryid <- tryCatch(pow_lookup(x), error = function(e) e)
  !inherits(tryid, "error")
}

#' @export
#' @rdname get_pow
get_pow_ <- function(x, messages = TRUE, rows = NA, ...){
  stats::setNames(lapply(x, get_pow_help, messages = messages, rows = rows, ...), x)
}

get_pow_help <- function(x, messages, rows, ...){
  mssg(messages, "\nRetrieving data for taxon '", x, "'\n")
  df <- pow_search(q = x, ...)$data
  if (NROW(df) == 0) NULL else sub_rows(df, rows)
}
