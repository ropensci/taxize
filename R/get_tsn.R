#' Get the TSN code for a search term.
#'
#' Retrieve the taxonomic serial numbers (TSN) of a taxon from ITIS.
#'
#' @export
#' @param searchterm character; A vector of common or scientific names.
#' @param searchtype character; One of 'scientific' or 'common', or any unique abbreviation
#' @param accepted logical; If TRUE, removes names that are not accepted valid names
#' by ITIS. Set to \code{FALSE} (default) to give back both accepted and unaccepted names.
#' @param ask logical; should get_tsn be run in interactive mode?
#' If \code{TRUE} and more than one TSN is found for the species, the user is asked for
#' input. If \code{FALSE} NA is returned for multiple matches.
#' @param verbose logical; should progress be printed?
#' @param rows numeric; Any number from 1 to inifity. If the default NA, all rows are considered.
#' Note that this function still only gives back a tsn class object with one to many identifiers.
#' See \code{\link[taxize]{get_tsn_}} to get back all, or a subset, of the raw data that you are
#' presented during the ask process.
#' @param x Input to as.tsn
#' @param ... Ignored
#' @param check logical; Check if ID matches any existing on the DB, only used in
#' \code{\link{as.tsn}}
#' @template getreturn
#'
#' @seealso \code{\link[taxize]{get_uid}}, \code{\link[taxize]{get_tsn}},
#' \code{\link[taxize]{get_gbifid}}, \code{\link[taxize]{get_tpsid}},
#' \code{\link[taxize]{get_eolid}}, \code{\link[taxize]{get_colid}},
#' \code{\link[taxize]{get_ids}}, \code{\link[taxize]{classification}}
#'
#' @examples \dontrun{
#' get_tsn(searchterm = "Quercus douglasii")
#' get_tsn(searchterm = "Chironomus riparius")
#' get_tsn(c("Chironomus riparius","Quercus douglasii"))
#' splist <- c("annona cherimola", 'annona muricata', "quercus robur",
#' 		"shorea robusta", "pandanus patina", "oryza sativa", "durio zibethinus")
#' get_tsn(splist, verbose=FALSE)
#'
#' # specify rows to limit choices available
#' get_tsn('Arni')
#' get_tsn('Arni', rows=1)
#' get_tsn('Arni', rows=1:2)
#'
#' # When not found
#' get_tsn("howdy")
#' get_tsn(c("Chironomus riparius", "howdy"))
#'
#' # Using common names
#' get_tsn(searchterm="black bear", searchtype="c")
#'
#' # Convert a tsn without class information to a tsn class
#' as.tsn(get_tsn("Quercus douglasii")) # already a tsn, returns the same
#' as.tsn(get_tsn(c("Chironomus riparius","Pinus contorta"))) # same
#' as.tsn(19322) # numeric
#' as.tsn(c(19322,129313,506198)) # numeric vector, length > 1
#' as.tsn("19322") # character
#' as.tsn(c("19322","129313","506198")) # character vector, length > 1
#' as.tsn(list("19322","129313","506198")) # list, either numeric or character
#' ## dont check, much faster
#' as.tsn("19322", check=FALSE)
#' as.tsn(19322, check=FALSE)
#' as.tsn(c("19322","129313","506198"), check=FALSE)
#' as.tsn(list("19322","129313","506198"), check=FALSE)
#'
#' (out <- as.tsn(c(19322,129313,506198)))
#' data.frame(out)
#' as.tsn( data.frame(out) )
#'
#' # Get all data back
#' get_tsn_("Arni")
#' get_tsn_("Arni", rows=1)
#' get_tsn_("Arni", rows=1:2)
#' get_tsn_(c("asdfadfasd","Pinus contorta"), rows=1:5)
#'
#' # use curl options
#' library("httr")
#' get_tsn("Quercus douglasii", config=verbose())
#' bb <- get_tsn("Quercus douglasii", config=progress())
#' }

get_tsn <- function(searchterm, searchtype = "scientific", accepted = FALSE, ask = TRUE,
  verbose = TRUE, rows = NA, ...)
{
  fun <- function(x, searchtype, ask, verbose, ...)
  {
    direct <- FALSE
    mssg(verbose, "\nRetrieving data for taxon '", x, "'\n")

    searchtype <- match.arg(searchtype, c("scientific","common"))
    tsn_df <- itis_terms(x, what = searchtype, ...)
    mm <- NROW(tsn_df) > 1
    tsn_df <- sub_rows(tsn_df, rows)

    if (!class(tsn_df) == "data.frame" || NROW(tsn_df) == 0) {
      tsn <- NA_character_
      att <- "not found"
    } else {
      tsn_df <- tsn_df[,c("tsn","scientificname","commonnames","nameusage")]

      if (accepted) {
        tsn_df <- tsn_df[ tsn_df$nameusage %in% c('valid','accepted'), ]
      }

      # should return NA if spec not found
      if (nrow(tsn_df) == 0) {
        mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
        tsn <- NA_character_
        att <- 'not found'
      }

      # take the one tsn from data.frame
      if (nrow(tsn_df) == 1) {
        tsn <- tsn_df$tsn
        att <- 'found'
      }

      # check for direct match
      if (nrow(tsn_df) > 1) {

        names(tsn_df)[grep(searchtype, names(tsn_df))] <- "target"
        direct <- match(tolower(tsn_df$target), tolower(x))

        if (!all(is.na(direct))) {
          tsn <- tsn_df$tsn[!is.na(direct)]
          direct <- TRUE
          att <- 'found'
        } else {
          tsn <- NA_character_
          att <- 'not found'
        }
      }

      # multiple matches
      if (any(
        nrow(tsn_df) > 1 && is.na(tsn) |
        nrow(tsn_df) > 1 && att == "found" & length(tsn) > 1
      )) {
        if (ask) {
          names(tsn_df)[grep(searchtype, names(tsn_df))] <- "target"
          # user prompt
          tsn_df <- tsn_df[order(tsn_df$target), ]
          rownames(tsn_df) <- 1:nrow(tsn_df)

          # prompt
          message("\n\n")
          print(tsn_df)
          message("\nMore than one TSN found for taxon '", x, "'!\n
            Enter rownumber of taxon (other inputs will return 'NA'):\n") # prompt
          take <- scan(n = 1, quiet = TRUE, what = 'raw')

          if (length(take) == 0) {
            take <- 'notake'
            att <- 'nothing chosen'
          }
          if (take %in% seq_len(nrow(tsn_df))) {
            take <- as.numeric(take)
            message("Input accepted, took taxon '", as.character(tsn_df$target[take]), "'.\n")
            tsn <-  tsn_df$tsn[take]
            att <- 'found'
          } else {
            tsn <- NA_character_
            mssg(verbose, "\nReturned 'NA'!\n\n")
            att <- 'not found'
          }
        } else {
          tsn <- NA_character_
          att <- "NA due to ask=FALSE"
        }
      }

    }

    data.frame(
      tsn = as.character(tsn),
      att = att,
      multiple = mm,
      direct = direct,
      stringsAsFactors = FALSE)
  }
  searchterm <- as.character(searchterm)
  outd <- ldply(searchterm, fun, searchtype, ask, verbose, ...)
  out <- outd$tsn
  attr(out, 'match') <- outd$att
  attr(out, 'multiple_matches') <- outd$multiple
  attr(out, 'pattern_match') <- outd$direct
  if ( !all(is.na(out)) ) {
    urlmake <- na.omit(out)
    attr(out, 'uri') <-
      sprintf('http://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=%s', urlmake)
  }
  class(out) <- "tsn"
  return(out)
}

#' @export
#' @rdname get_tsn
as.tsn <- function(x, check=TRUE) UseMethod("as.tsn")

#' @export
#' @rdname get_tsn
as.tsn.tsn <- function(x, check=TRUE) x

#' @export
#' @rdname get_tsn
as.tsn.character <- function(x, check=TRUE) if(length(x) == 1) make_tsn(x, check) else collapse(x, make_tsn, "tsn", check = check)

#' @export
#' @rdname get_tsn
as.tsn.list <- function(x, check=TRUE) if (length(x) == 1) make_tsn(x, check) else collapse(x, make_tsn, "tsn", check = check)

#' @export
#' @rdname get_tsn
as.tsn.numeric <- function(x, check=TRUE) as.tsn(as.character(x), check)

#' @export
#' @rdname get_tsn
as.tsn.data.frame <- function(x, check=TRUE) {
  structure(x$ids, class = "tsn", match = x$match,
            multiple_matches = x$multiple_matches,
            pattern_match = x$pattern_match, uri = x$uri)
}

#' @export
#' @rdname get_tsn
as.data.frame.tsn <- function(x, ...){
  data.frame(ids = as.character(unclass(x)),
             class = "tsn",
             match = attr(x, "match"),
             multiple_matches = attr(x, "multiple_matches"),
             pattern_match = attr(x, "pattern_match"),
             uri = attr(x, "uri"),
             stringsAsFactors = FALSE)
}

make_tsn <- function(x, check=TRUE) make_generic(x, 'http://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=%s', "tsn", check)

check_tsn <- function(x){
  tt <- suppressMessages(itis_getrecord(x))
  identical(tt$acceptednamelist[[1]], as.character(x))
}

#' @export
#' @rdname get_tsn
get_tsn_ <- function(searchterm, verbose = TRUE, searchtype = "scientific", accepted = TRUE, rows = NA){
  setNames(lapply(searchterm, get_tsn_help, verbose = verbose, searchtype=searchtype, accepted=accepted, rows=rows), searchterm)
}

get_tsn_help <- function(searchterm, verbose, searchtype, accepted, rows){
  mssg(verbose, "\nRetrieving data for taxon '", searchterm, "'\n")
  searchtype <- match.arg(searchtype, c("scientific","common"))
  df <- itis_terms(searchterm, what = searchtype, verbose = verbose)
  if (!is(df, "data.frame") || NROW(df) == 0) {
    NULL
  } else {
    df <- df[,c("tsn","scientificname","commonnames","nameusage")]
    if (accepted) df <- df[ df$nameusage %in% c('valid','accepted'), ]
    sub_rows(df, rows)
  }
}
