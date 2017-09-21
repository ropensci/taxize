#' Get NatureServe taxonomic ID for a taxon name
#'
#' @export
#' @param query character; A vector of common or scientific names.
#' @param searchtype character; One of 'scientific' (default) or 'common'.
#' This doesn't affect the query to NatureServe - but rather affects what
#' column of data is targeted in name filtering post data request.
#' @param ask logical; should get_natservid be run in interactive mode?
#' If \code{TRUE} and more than one wormsid is found for the species, the
#' user is asked for input. If \code{FALSE} NA is returned for
#' multiple matches.
#' @param verbose logical; should progress be printed?
#' @param rows numeric; Any number from 1 to infinity. If the default NaN, all
#' rows are considered. Note that this function still only gives back a
#' natservid class object with one to many identifiers. See
#' \code{\link[taxize]{get_natservid_}} to get back all, or a subset, of the raw
#' data that you are presented during the ask process.
#' @param key (character) your NatureServe API key. Required. See
#' \strong{Authentication} below for more.
#' @param x Input to as.natservid
#' @param ... Ignored
#' @param check logical; Check if ID matches any existing on the DB, only
#' used in \code{\link{as.natservid}}
#' @template getreturn
#'
#' @family taxonomic-ids
#' @seealso \code{\link[taxize]{classification}}
#'
#' @section Authentication:
#' Get an API key from NatureServe at
#' \url{https://services.natureserve.org/developer/index.jsp}.
#' You can pass your token in as an argument or store it one of two places:
#'
#' \itemize{
#'   \item your .Rprofile file with an entry like
#'   \code{options(NatureServeKey = "your-natureserve-key")}
#'   \item your .Renviron file with an entry like
#'   \code{NATURE_SERVE_KEY=your-natureserve-key}
#' }
#'
#' See \code{\link{Startup}} for information on how to create/find your
#' .Rprofile and .Renviron files
#'
#' @examples \dontrun{
#' (x <- get_natservid("Helianthus annuus"))
#' attributes(x)
#' attr(x, "match")
#' attr(x, "multiple_matches")
#' attr(x, "pattern_match")
#' attr(x, "uri")
#'
#' get_natservid('Gadus morhua')
#' get_natservid(c("Helianthus annuus", 'Gadus morhua'))
#'
#' # specify rows to limit choices available
#' get_natservid('Ruby Quaker Moth', 'common')
#' get_natservid('Ruby*', 'common')
#' get_natservid('Ruby*', 'common', rows=1)
#' get_natservid('Ruby*', 'common', rows=1:2)
#'
#' # When not found
#' get_natservid("howdy")
#' get_natservid(c('Gadus morhua', "howdy"))
#'
#' # Convert a natservid without class information to a natservid class
#' # already a natservid, returns the same
#' as.natservid(get_natservid('Gadus morhua'))
#' # same
#' as.natservid(get_natservid(c('Gadus morhua', 'Pomatomus saltatrix')))
#' # character
#' as.natservid("ELEMENT_GLOBAL.2.101905")
#' # character vector, length > 1
#' as.natservid(c("ELEMENT_GLOBAL.2.101905", "ELEMENT_GLOBAL.2.101998"))
#' # list, either numeric or character
#' as.natservid(list("ELEMENT_GLOBAL.2.101905", "ELEMENT_GLOBAL.2.101998"))
#' ## dont check, much faster
#' as.natservid("ELEMENT_GLOBAL.2.101905", check = FALSE)
#' as.natservid(c("ELEMENT_GLOBAL.2.101905", "ELEMENT_GLOBAL.2.101998"),
#'   check = FALSE)
#' as.natservid(list("ELEMENT_GLOBAL.2.101905", "ELEMENT_GLOBAL.2.101998"),
#'   check = FALSE)
#'
#' (out <- as.natservid(
#'   c("ELEMENT_GLOBAL.2.101905", "ELEMENT_GLOBAL.2.101998")))
#' data.frame(out)
#' as.natservid( data.frame(out) )
#'
#' # Get all data back
#' get_natservid_("Ruby*")
#' get_natservid_("Ruby*", rows=1:3)
#' }
get_natservid <- function(query, searchtype = "scientific", ask = TRUE,
                          verbose = TRUE, rows = NA, key = NULL, ...) {

  assert(ask, "logical")
  assert(searchtype, "character")
  assert(ask, "logical")
  assert(verbose, "logical")

  fun <- function(x, searchtype, ask, verbose, key, rows, ...) {
    direct <- FALSE
    mssg(verbose, "\nRetrieving data for taxon '", x, "'\n")

    if (!searchtype %in% c("scientific", "common")) {
      stop("'searchtype' must be one of 'scientific' or 'common'", call. = FALSE)
    }

    nsdf <- ns_worker(x, key, ...)
    mm <- NROW(nsdf) > 1

    if (!inherits(nsdf, "tbl_df") || NROW(nsdf) == 0) {
      nsid <- NA_character_
      att <- "not found"
    } else {
      nsdf <- suppressWarnings(data.frame(nsdf))
      nsdf <- sub_rows(nsdf, rows)

      # should return NA if spec not found
      if (nrow(nsdf) == 0) {
        mssg(
          verbose,
          "Not found. Consider checking the spelling or alternate classification")
        nsid <- NA_character_
        att <- 'not found'
      }

      # take the one nsid from data.frame
      if (nrow(nsdf) == 1) {
        nsid <- nsdf$id
        att <- 'found'
      }

      # check for direct match
      if (nrow(nsdf) > 1) {

        names(nsdf)[grep(searchtype, names(nsdf))] <- "target"
        direct <- match(tolower(nsdf$target), tolower(x))

        if (length(direct) == 1) {
          if (!all(is.na(direct))) {
            nsid <- nsdf$id[!is.na(direct)]
            direct <- TRUE
            att <- 'found'
          } else {
            direct <- FALSE
            nsid <- NA_character_
            att <- 'not found'
          }
        } else {
          direct <- FALSE
          nsid <- NA_character_
          att <- 'NA due to ask=FALSE & no direct match found'
          warning("> 1 result; no direct match found", call. = FALSE)
        }
      }

      # multiple matches
      if (any(
        nrow(nsdf) > 1 && is.na(nsid) ||
        nrow(nsdf) > 1 && att == "found" && length(nsid) > 1
      )) {
        if (ask) {
          names(nsdf)[grep(searchtype, names(nsdf))] <- "target"

          # user prompt
          nsdf <- nsdf[order(nsdf$target), ]

          # prompt
          message("\n\n")
          rownames(nsdf) <- seq_len(NROW(nsdf))
          print(nsdf)
          message("\nMore than one NatureServe ID found for taxon '", x, "'!\n
                  Enter rownumber of taxon (other inputs will return 'NA'):\n") # prompt
          take <- scan(n = 1, quiet = TRUE, what = 'raw')

          if (length(take) == 0) {
            take <- 'notake'
            att <- 'nothing chosen'
          }
          if (take %in% seq_len(nrow(nsdf))) {
            take <- as.numeric(take)
            message("Input accepted, took taxon '", as.character(nsdf$target[take]), "'.\n")
            nsid <-  nsdf$id[take]
            att <- 'found'
          } else {
            nsid <- NA_character_
            mssg(verbose, "\nReturned 'NA'!\n\n")
            att <- 'not found'
          }
        } else {
          if (length(nsid) != 1) {
            warning(
              sprintf("More than one NatureServe ID found for taxon '%s'; refine query or set ask=TRUE",
                      x),
              call. = FALSE
            )
            nsid <- NA_character_
            att <- 'NA due to ask=FALSE & > 1 result'
          }
        }
      }

    }

    data.frame(
      nsid = as.character(nsid),
      att = att,
      multiple = mm,
      direct = direct,
      stringsAsFactors = FALSE)
  }
  query <- as.character(query)
  outd <- ldply(query, fun, searchtype = searchtype, ask = ask,
                verbose = verbose, key = key, rows = rows, ...)
  out <- outd$nsid
  attr(out, 'match') <- outd$att
  attr(out, 'multiple_matches') <- outd$multiple
  attr(out, 'pattern_match') <- outd$direct
  if ( !all(is.na(out)) ) {
    urlmake <- na.omit(out)
    attr(out, 'uri') <- sprintf(ns_base_uri(), urlmake)
  }
  class(out) <- "natservid"
  return(out)
}

#' @export
#' @rdname get_natservid
as.natservid <- function(x, check=TRUE) UseMethod("as.natservid")

#' @export
#' @rdname get_natservid
as.natservid.natservid <- function(x, check=TRUE) x

#' @export
#' @rdname get_natservid
as.natservid.character <- function(x, check=TRUE) if (length(x) == 1) make_natserv(x, check) else collapse(x, make_natserv, "natservid", check = check)

#' @export
#' @rdname get_natservid
as.natservid.list <- function(x, check=TRUE) if (length(x) == 1) make_natserv(x, check) else collapse(x, make_natserv, "natservid", check = check)

#' @export
#' @rdname get_natservid
as.natservid.numeric <- function(x, check=TRUE) as.natservid(as.character(x), check)

#' @export
#' @rdname get_natservid
as.natservid.data.frame <- function(x, check=TRUE) {
  structure(x$ids, class = "natservid", match = x$match,
            multiple_matches = x$multiple_matches,
            pattern_match = x$pattern_match, uri = x$uri)
}

#' @export
#' @rdname get_natservid
as.data.frame.natservid <- function(x, ...){
  data.frame(ids = as.character(unclass(x)),
             class = "natservid",
             match = attr(x, "match"),
             multiple_matches = attr(x, "multiple_matches"),
             pattern_match = attr(x, "pattern_match"),
             uri = attr(x, "uri"),
             stringsAsFactors = FALSE)
}

make_natserv <- function(x, check=TRUE) make_generic(x, ns_base_uri(), "natservid", check)

check_natservid <- function(x){
  tt <- httr::content(httr::GET(sprintf(ns_base_uri(), x)), "text")
  !grepl("No records matched", tt)
}

#' @export
#' @rdname get_natservid
get_natservid_ <- function(query, verbose = TRUE, rows = NA, key = NULL, ...) {
  stats::setNames(
    lapply(query, get_natservid_help, verbose = verbose, rows = rows,
           key = key, ...),
    query
  )
}

get_natservid_help <- function(query, verbose, rows, key, ...) {
  mssg(verbose, "\nRetrieving data for taxon '", query, "'\n")
  df <- ns_worker(query, key, ...)
  sub_rows(df, rows)
}

ns_base_uri <- function() "http://explorer.natureserve.org/servlet/NatureServe?searchSpeciesUid=%s"

ns_worker <- function(x, key, ...) {
  tmp <- tryCatch(natserv::ns_search(x = x, key = key, ...), error = function(e) e)
  if (inherits(tmp, "error")) return(tibble::data_frame())
  tmp <- tmp[, c("globalSpeciesUid","jurisdictionScientificName","commonName","natureServeExplorerURI")]
  names(tmp) <- c('id', 'scientificname', 'commonname', 'uri')
  tmp
}
