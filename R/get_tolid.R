#' Get the OTT id for a search term
#'
#' Retrieve the Open Tree of Life Taxonomy (OTT) id of a taxon from
#' OpenTreeOfLife
#'
#' @export
#' @inheritParams tol_resolve
#' @param sciname character; scientific name.
#' @param ask logical; should \code{get_tolid} be run in interactive mode?
#' If \code{TRUE} and more than one TOL is found for the species, the user is
#' asked for input. If \code{FALSE} NA is returned for multiple matches.
#' @param verbose logical; should progress be printed?
#' @param rows numeric; Any number from 1 to infinity. If the default NA, all
#' rows are considered. Note that this function still only gives back a tol
#' class object with one to many identifiers. See \code{\link[taxize]{get_tolid_}}
#' to get back all, or a subset, of the raw data that you are presented during
#' the ask process.
#' @param x Input to \code{as.tolid}
#' @param ... Ignored
#' @param check logical; Check if ID matches any existing on the DB, only
#' used in
#' \code{\link{as.tolid}}
#' @template getreturn
#'
#' @family taxonomic-ids
#' @seealso \code{\link[taxize]{classification}}
#'
#' @examples \dontrun{
#' get_tolid(sciname = "Quercus douglasii")
#' get_tolid(sciname = "Chironomus riparius")
#' get_tolid(c("Chironomus riparius","Quercus douglasii"))
#' splist <- c("annona cherimola", 'annona muricata', "quercus robur",
#' 		"shorea robusta", "pandanus patina", "oryza sativa", "durio zibethinus")
#' get_tolid(splist, verbose=FALSE)
#'
#' # specify rows to limit choices available
#' get_tolid('Arni')
#' get_tolid('Arni', rows=1)
#' get_tolid('Arni', rows=1:2)
#'
#' # When not found
#' get_tolid("howdy")
#' get_tolid(c("Chironomus riparius", "howdy"))
#'
#' # Convert a tol without class information to a tol class
#' as.tolid(get_tolid("Quercus douglasii")) # already a tol, returns the same
#' as.tolid(get_tolid(c("Chironomus riparius","Pinus contorta"))) # same
#' as.tolid(3930798) # numeric
#' as.tolid(c(3930798,515712,872577)) # numeric vector, length > 1
#' as.tolid("3930798") # character
#' as.tolid(c("3930798","515712","872577")) # character vector, length > 1
#' as.tolid(list("3930798","515712","872577")) # list, either numeric or character
#' ## dont check, much faster
#' as.tolid("3930798", check=FALSE)
#' as.tolid(3930798, check=FALSE)
#' as.tolid(c("3930798","515712","872577"), check=FALSE)
#' as.tolid(list("3930798","515712","872577"), check=FALSE)
#'
#' (out <- as.tolid(c(3930798,515712,872577)))
#' data.frame(out)
#' as.tolid( data.frame(out) )
#'
#' # Get all data back
#' get_tolid_(sciname="Arni")
#' get_tolid_("Arni", rows=1)
#' get_tolid_("Arni", rows=1:2)
#' get_tolid_(c("asdfadfasd","Pinus contorta"))
#' }

get_tolid <- function(sciname, ask = TRUE, verbose = TRUE, rows = NA, ...) {
  fun <- function(x, ask, verbose, ...) {
    mssg(verbose, "\nRetrieving data for taxon '", x, "'\n")

    tol_df <- tryCatch(tol_resolve(x, ...), error = function(e) e)

    if (!inherits(tol_df, "data.frame") || NROW(tol_df) == 0 || inherits(tol_df, "error")) {
      id <- NA_character_
      att <- "not found"
    } else {
      tol_df <- tol_fetch_fuzzy(tol_df)
      tol_df <- sub_rows(tol_df, rows)

      # should return NA if spec not found
      if (NROW(tol_df) == 0) {
        mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
        id <- NA_character_
        att <- 'not found'
      }

      # take the one tol from data.frame
      if (NROW(tol_df) == 1) {
        id <- tol_df$ott_id
        att <- 'found'
      }

      # more than one found -> user input
      if (NROW(tol_df) > 1) {
        # check for exact match
        matchtmp <- tol_df[tol_df$unique_name %in% x, "ott_id"]
        if (length(matchtmp) == 1) {
          id <- as.character(matchtmp)
          direct <- TRUE
        } else {
          if (ask) {
            tol_df <- sub_rows(tol_df, rows)
            if (NROW(tol_df) == 0) {
              id <- NA_character_
              att <- "not found"
            } else {
              id <- tol_df$ott_id
              if (length(id) == 1) {
                rank_taken <- tol_df$rank
                att <- "found"
              }
            }

            if (length(id) > 1) {
              # prompt
              message("\n\n")
              message("\nMore than one tol ID found for taxon '", x, "'!\n
                      Enter rownumber of taxon (other inputs will return 'NA'):\n")
              rownames(tol_df) <- 1:nrow(tol_df)
              print(tol_df)
              take <- scan(n = 1, quiet = TRUE, what = 'raw')

              if (length(take) == 0) {
                take <- 'notake'
                att <- 'nothing chosen'
              }
              if (take %in% seq_len(nrow(tol_df))) {
                take <- as.numeric(take)
                message("Input accepted, took tol ID '", as.character(tol_df$ott_id[take]), "'.\n")
                id <- as.character(tol_df$ott_id[take])
                att <- "found"
              } else {
                id <- NA_character_
                att <- "not found"
                mssg(verbose, "\nReturned 'NA'!\n\n")
              }
            }
          } else {
            id <- NA_character_
            att <- "NA due to ask=FALSE"
          }
        }
      }
    }

    data.frame(
      tol = as.character(id),
      att = att,
      multiple = FALSE,
      direct = FALSE,
      stringsAsFactors = FALSE)
  }
  sciname <- as.character(sciname)
  outd <- ldply(sciname, fun, ask, verbose, ...)
  out <- outd$tol
  attr(out, 'match') <- outd$att
  attr(out, 'multiple_matches') <- outd$multiple
  attr(out, 'pattern_match') <- outd$direct
  if ( !all(is.na(out)) ) {
    urlmake <- na.omit(out)
    attr(out, 'uri') <-
      paste0('https://tree.opentreeoflife.org/opentree/argus/ottol@', urlmake)
  }
  class(out) <- "tolid"
  return(out)
}

#' @export
#' @rdname get_tolid
as.tolid <- function(x, check=TRUE) UseMethod("as.tolid")

#' @export
#' @rdname get_tolid
as.tolid.tolid <- function(x, check=TRUE) x

#' @export
#' @rdname get_tolid
as.tolid.character <- function(x, check=TRUE) {
  if (length(x) == 1) make_tol(x, check) else collapse(x, make_tol, "tolid", check = check)
}

#' @export
#' @rdname get_tolid
as.tolid.list <- function(x, check=TRUE) {
  if (length(x) == 1) make_tol(x, check) else collapse(x, make_tol, "tolid", check = check)
}

#' @export
#' @rdname get_tolid
as.tolid.numeric <- function(x, check=TRUE) as.tolid(as.character(x), check)

#' @export
#' @rdname get_tolid
as.tolid.data.frame <- function(x, check=TRUE) {
  structure(x$ids, class = "tolid", match = x$match,
            multiple_matches = x$multiple_matches,
            pattern_match = x$pattern_match, uri = x$uri)
}

#' @export
#' @rdname get_tolid
as.data.frame.tolid <- function(x, ...){
  data.frame(ids = as.character(unclass(x)),
             class = "tolid",
             match = attr(x, "match"),
             multiple_matches = attr(x, "multiple_matches"),
             pattern_match = attr(x, "pattern_match"),
             uri = attr(x, "uri"),
             stringsAsFactors = FALSE)
}

make_tol <- function(x, check=TRUE) {
  make_generic(x, 'https://tree.opentreeoflife.org/opentree/argus/ottol@%s', "tolid", check)
}

check_tolid <- function(x){
  tt <- tryCatch(rotl::taxonomy_taxon_info(x), error = function(e) e)
  !inherits(tt, "error")
}

#' @export
#' @rdname get_tolid
get_tolid_ <- function(sciname, verbose = TRUE, rows = NA){
  stats::setNames(
    lapply(sciname, get_tolid_help, verbose = verbose, rows = rows),
    sciname
  )
}

get_tolid_help <- function(sciname, verbose, rows, ...){
  mssg(verbose, "\nRetrieving data for taxon '", sciname, "'\n")
  tol_df <- tryCatch(tol_resolve(sciname, ...), error = function(e) e)
  if (!inherits(tol_df, "data.frame") || NROW(tol_df) == 0 || inherits(tol_df, "error")) {
    NULL
  } else {
    df <- tol_fetch_fuzzy(tol_df)
    sub_rows(df, rows)
  }
}

tol_fetch_fuzzy <- function(x) {
  atts <- attr(x, "original_response")
  df <- dtrbsetdf(lapply(atts$results, function(z) {
    dtrbsetdf(lapply(z$matches, function(w) {
      c(
        pop(w, "taxon"),
        lapply(w$taxon, function(m) {
          if (length(m) > 1) {
            paste0(m, collapse = ",")
          } else if (length(m) == 0) {
            ""
          } else if (length(m) == 1) {
            m[[1]]
          } else {
            m
          }
        })
      )
    }))
  }))
  df <- move_col_begin(df, "matched_name")
  df <- move_col_begin(df, "unique_name")
  df
}

dtrbsetdf <- function(x) {
  (out <- data.table::setDF(
    data.table::rbindlist(x, use.names = TRUE, fill = TRUE)
  ))
}
