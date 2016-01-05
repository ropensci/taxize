#' Get the uBio id for a search term
#'
#' THIS FUNCTION IS DEFUNCT.
#'
#' @rdname get_ubioid-defunct
#' @export
#' @param searchterm character; A vector of common or scientific names.
#' @param searchtype character; One of 'scientific' or 'common', or any unique abbreviation
#' @param ask logical; should get_tsn be run in interactive mode?
#' If TRUE and more than one TSN is found for teh species, the user is asked for
#' input. If FALSE NA is returned for multiple matches.
#' @param verbose logical; should progress be printed?
#' @param rows numeric; Any number from 1 to inifity. If the default NA, all rows are considered.
#' Note that this function still only gives back a ubioid class object with one to many identifiers.
#' See \code{\link[taxize]{get_ubioid_}} to get back all, or a subset, of the raw data that you are
#' presented during the ask process.
#' @param family (character) A family name. Optional. See \code{Filtering} below.
#' @param rank (character) A taxonomic rank name. See \code{\link{rank_ref}} for possible
#' options. Though note that some data sources use atypical ranks, so inspect the
#' data itself for options. Optional. See \code{Filtering} below.
#' @param x Input to \code{\link{as.ubioid}}
#' @param ... Ignored
#' @param check logical; Check if ID matches any existing on the DB, only used in
#' \code{\link{as.ubioid}}
#'
#' @return A vector of uBio ids. If a taxon is not found NA is given. If more than one uBio
#'    id is found the function asks for user input (if ask = TRUE), otherwise returns NA.
#'    Comes with an attribute \emph{match} to investigate the reason for NA (either 'not found',
#'    'found' or if ask = FALSE 'NA due to ask=FALSE')
#'
#' @section Filtering:
#' The parameters \code{family} and \code{rank} are not used in the search to the data
#' provider, but are used in filtering the data down to a subset that is closer to the
#' target you want.  For all these parameters,
#' you can use regex strings since we use \code{\link{grep}} internally to match.
#' Filtering narrows down to the set that matches your query, and removes the rest.
#'
#' @seealso \code{\link[taxize]{get_uid}}, \code{\link[taxize]{ubio_search}}
get_ubioid <- function(searchterm, searchtype = "scientific", ask = TRUE, verbose = TRUE,
                       rows = NA, family = NULL, rank = NULL, ...) {
  .Defunct(msg = "the uBio API is down, for good as far as we know")

  fun <- function(x, searchtype, ask, verbose, rows, ...) {
    mssg(verbose, "\nRetrieving data for taxon '", x, "'\n")

    searchtype <- match.arg(searchtype, c("scientific","common"))
    if (searchtype == 'scientific') {
      sci <- 1; vern <- 0
    } else {
      sci <- 0; vern <- 1; searchtype = 'vernacular'
    }
    ubio_df <- tryCatch(ubio_search(searchName = x, sci = sci, vern = vern, ...)[[searchtype]], error = function(e) e)
    ubio_df <- sub_rows(ubio_df, rows)

    if (is(ubio_df, "simpleError")) {
      ubioid <- NA
      att <- "not found"
    } else {
      ubio_df <- switch(searchtype,
                        scientific = ubio_df[,c("namebankid","namestring","packagename","rankname")],
                        vernacular = ubio_df[,c("namebankid","namestring","packagename")])
      if (searchtype == "scientific") {
        ubio_df <- rename(ubio_df, c('packagename' = 'family',
                                     'rankname' = 'rank',
                                     'namebankid' = 'ubioid'))
      } else {
        ubio_df <- rename(ubio_df, c('packagename' = 'family',
                                     'namebankid' = 'ubioid'))
      }
      if (searchtype == "scientific") ubio_df <- fix_ranks(ubio_df)

      direct <- NA
      # should return NA if spec not found
      if (nrow(ubio_df) == 0) {
        mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
        ubioid <- NA
        att <- 'not found'
      }
      # take the one ubio id from data.frame
      if (nrow(ubio_df) == 1) {
        ubioid <- ubio_df$ubioid
        att <- 'found'
      }
      # check for direct match
      if (nrow(ubio_df) > 1) {
        names(ubio_df)[grep('namestring', names(ubio_df))] <- "target"
        direct <- match(tolower(ubio_df$target), tolower(x))
        if (length(na.omit(direct)) == 1) {
          if (!all(is.na(direct))) {
            ubioid <- ubio_df$ubioid[!is.na(direct)]
            att <- 'found'
          } else {
            ubioid <- NA
            direct <- NA
            att <- 'not found'
          }
        } else {
          ubioid <- ubio_df$ubioid
          att <- 'found'
        }
      }
      # multiple matches
      if (any(
        nrow(ubio_df) > 1 & is.na(ubioid) |
        nrow(ubio_df) > 1 & att == "found" & length(ubioid) > 1
      )) {
        if (ask) {
          names(ubio_df)[names(ubio_df) %in% "namestring"] <- "target"
          # user prompt
          ubio_df <- ubio_df[order(ubio_df$target), ]
          id <- ubio_df$ubioid

          if (!is.null(family) || !is.null(rank)) {
            ubio_df <- filt(ubio_df, "family", family)
            if (searchtype == "scientific") ubio_df <- filt(ubio_df, "rank", rank)
            if (NROW(ubio_df) > 1) rownames(ubio_df) <- 1:nrow(ubio_df)
            ubioid <- id <- ubio_df$ubioid
            if (length(id) == 1) {
              att <- "found"
            }
          }

          if (length(id) > 1) {
            # prompt
            message("\n\n")
            rownames(ubio_df) <- 1:nrow(ubio_df)
            print(ubio_df)
            message("\nMore than one uBio ID found for taxon '", x, "'!\n
            Enter rownumber of taxon (other inputs will return 'NA'):\n") # prompt
            take <- scan(n = 1, quiet = TRUE, what = 'raw')

            if (length(take) == 0) {
              take <- 'notake'
              att <- 'nothing chosen'
            }
            if (take %in% seq_len(nrow(ubio_df))) {
              take <- as.numeric(take)
              message("Input accepted, took taxon '", as.character(ubio_df$target[take]), "'.\n")
              ubioid <-  ubio_df$ubioid[take]
              att <- 'found'
            } else {
              ubioid <- NA
              mssg(verbose, "\nReturned 'NA'!\n\n")
              att <- 'not found'
            }
          }
        } else {
          ubioid <- NA
          att <- 'NA due to ask=FALSE'
        }
      }

    }
    return(data.frame(ubioid = as.character(ubioid), att = att, stringsAsFactors=FALSE))
  }
  searchterm <- as.character(searchterm)
  outd <- ldply(searchterm, fun, searchtype, ask, verbose, rows, ...)
  out <- structure(outd$ubioid, class="ubioid", match=outd$att)
  add_uri(out, 'http://www.ubio.org/browser/details.php?namebankID=%s')
}


#' @export
#' @rdname get_ubioid-defunct
as.ubioid <- function(x, check=TRUE) UseMethod("as.ubioid")

#' @export
#' @rdname get_ubioid-defunct
as.ubioid.ubioid <- function(x, check=TRUE) {
  .Defunct(msg = "the uBio API is down, for good as far as we know")
  x
}

#' @export
#' @rdname get_ubioid-defunct
as.ubioid.character <- function(x, check=TRUE) {
  .Defunct(msg = "the uBio API is down, for good as far as we know")
  if(length(x) == 1) make_ubioid(x, check) else collapse(x, make_ubioid, "ubioid", check=check)
}

#' @export
#' @rdname get_ubioid-defunct
as.ubioid.list <- function(x, check=TRUE) {
  .Defunct(msg = "the uBio API is down, for good as far as we know")
  if(length(x) == 1) make_ubioid(x, check) else collapse(x, make_ubioid, "ubioid", check=check)
}

#' @export
#' @rdname get_ubioid-defunct
as.ubioid.numeric <- function(x, check=TRUE) {
  .Defunct(msg = "the uBio API is down, for good as far as we know")
  as.ubioid(as.character(x), check)
}

#' @export
#' @rdname get_ubioid-defunct
as.ubioid.data.frame <- function(x, check=TRUE) {
  .Defunct(msg = "the uBio API is down, for good as far as we know")
  structure(x$ids, class="ubioid", match=x$match, uri=x$uri)
}

#' @export
#' @rdname get_ubioid-defunct
as.data.frame.ubioid <- function(x, ...){
  .Defunct(msg = "the uBio API is down, for good as far as we know")
  data.frame(ids = as.character(unclass(x)),
             class = "ubioid",
             match = attr(x, "match"),
             uri = attr(x, "uri"),
             stringsAsFactors = FALSE)
}

make_ubioid <- function(x, check=TRUE) make_generic(x, 'http://www.ubio.org/browser/details.php?namebankID=%s', "ubioid", check)

check_ubioid <- function(x){
  res <- ubio_id(x)
  is(res$data, "data.frame")
}

#' @export
#' @rdname get_ubioid-defunct
get_ubioid_ <- function(searchterm, verbose = TRUE, searchtype = "scientific", rows = NA){
  .Defunct(msg = "the uBio API is down, for good as far as we know")
  setNames(lapply(searchterm, get_ubioid_help, verbose = verbose, searchtype=searchtype, rows=rows), searchterm)
}

get_ubioid_help <- function(searchterm, verbose, searchtype, rows){
  mssg(verbose, "\nRetrieving data for taxon '", searchterm, "'\n")
  searchtype <- match.arg(searchtype, c("scientific","common"))
  if(searchtype=='scientific'){ sci <- 1; vern <- 0 } else { sci <- 0; vern <- 1; searchtype='vernacular' }
  ubio_df <-  tryCatch(ubio_search(searchName = searchterm, sci = sci, vern = vern)[[searchtype]], error=function(e) e)
  if(is(ubio_df, "simpleError")){
    NULL
  } else {
    ubio_df <- switch(searchtype,
                      scientific=ubio_df[,c("namebankid","namestring","packagename","rankname")],
                      vernacular=ubio_df[,c("namebankid","namestring","packagename")])
    ubio_df <- rename(ubio_df, c('packagename' = 'family'))
    sub_rows(ubio_df, rows)
  }
}

fix_ranks <- function(x) {
  rr <- x$rank
  # repl <- c('SP','genus','species','gen','trinomial','sub-species','var','subspecies','ssp.','subsp')
  rr <- gsub("^SP$", "species", rr)
  rr <- gsub("^gen$", "genus", rr)
  rr <- gsub("^subsp$", "subspecies", rr)
  rr <- gsub("^sub-species$", "subspecies", rr)
  rr <- gsub("^ssp\\.$", "subspecies", rr)
  x$rank <- rr
  x
}
