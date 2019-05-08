#' Get NatureServe taxonomic ID for a taxon name
#'
#' @export
#' @param query character; A vector of common or scientific names. Or, a
#' [taxon_state()] object
#' @param searchtype character; One of 'scientific' (default) or 'common'.
#' This doesn't affect the query to NatureServe - but rather affects what
#' column of data is targeted in name filtering post data request.
#' @param ask logical; should get_natservid be run in interactive mode?
#' If `TRUE` and more than one wormsid is found for the species, the
#' user is asked for input. If `FALSE` NA is returned for
#' multiple matches.
#' @param messages logical; should progress be printed?
#' @param rows numeric; Any number from 1 to infinity. If the default NaN, all
#' rows are considered. Note that this function still only gives back a
#' natservid class object with one to many identifiers. See
#' [`get_natservid_()`] to get back all, or a subset, of the raw
#' data that you are presented during the ask process.
#' @param key (character) your NatureServe API key. Required. See
#' **Authentication** below for more.
#' @param x Input to as.natservid
#' @param ... Ignored
#' @param check logical; Check if ID matches any existing on the DB, only
#' used in [`as.natservid()`]
#' @template getreturn
#'
#' @family taxonomic-ids
#' @seealso [`classification()`]
#'
#' @section Authentication:
#' Get an API key from NatureServe at
#' <https://services.natureserve.org/developer/index.jsp>.
#' You can pass your token in as an argument or store it one of two places:

#' * your .Rprofile file with an entry like
#' `options(NatureServeKey = "your-natureserve-key")`
#' * your .Renviron file with an entry like
#' `NATURE_SERVE_KEY=your-natureserve-key`
#'
#' See [`Startup()`] for information on how to create/find your
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
                          messages = TRUE, rows = NA, key = NULL, ...) {

  assert(query, c("character", "taxon_state"))
  assert(ask, "logical")
  assert(searchtype, "character")
  assert(ask, "logical")
  assert(messages, "logical")
  assert_rows(rows)

  if (inherits(query, "character")) {
    tstate <- taxon_state$new(class = "natservid", names = query)
    items <- query
  } else {
    tstate <- query
    query <- tstate$taxa_remaining()
    items <- c(query, tstate$taxa_completed())
  }

  prog <- progressor$new(items = items, suppress = !messages)
  done <- tstate$get()
  for (i in seq_along(done)) prog$completed(names(done)[i], done[[i]]$att)
  prog$prog_start()

  for (i in seq_along(query)) {
    direct <- FALSE
    mssg(messages, "\nRetrieving data for taxon '", query[i], "'\n")

    if (!searchtype %in% c("scientific", "common")) {
      stop("'searchtype' must be one of 'scientific' or 'common'",
        call. = FALSE)
    }

    nsdf <- ns_worker(query[i], key, ...)
    mm <- NROW(nsdf) > 1

    if (!inherits(nsdf, "tbl_df") || NROW(nsdf) == 0) {
      nsid <- NA_character_
      att <- "not found"
    } else {
      nsdf <- suppressWarnings(data.frame(nsdf))
      nsdf <- sub_rows(nsdf, rows)

      # should return NA if spec not found
      if (nrow(nsdf) == 0) {
        mssg(messages, m_not_found_sp_altclass)
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
        direct <- match(tolower(nsdf$target), tolower(query[i]))

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
          att <- m_na_ask_false_no_direct
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
          message("\nMore than one NatureServe ID found for taxon '", query[i], "'!\n
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
            mssg(messages, "\nReturned 'NA'!\n\n")
            att <- 'not found'
          }
        } else {
          if (length(nsid) != 1) {
            warning(sprintf(m_more_than_one_found, "NatureServe ID", query[i]),
              call. = FALSE)
            nsid <- NA_character_
            att <- m_na_ask_false
          }
        }
      }

    }
    res <- list(id = as.character(nsid), att = att, multiple = mm,
      direct = direct)
    prog$completed(query[i], att)
    prog$prog(att)
    tstate$add(query[i], res)
  }
  out <- tstate$get()
  ids <- structure(as.character(unlist(pluck(out, "id"))), class = "natservid",
                   match = pluck_un(out, "att", ""),
                   multiple_matches = pluck_un(out, "multiple", logical(1)),
                   pattern_match = pluck_un(out, "direct", logical(1)))
  on.exit(prog$prog_summary(), add = TRUE)
  on.exit(tstate$exit, add = TRUE)
  add_uri(ids, get_url_templates$natserv)
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
  tt <- crul::HttpClient$new(sprintf(ns_base_uri(), x),
    headers = tx_ual)$get()$parse("UTF-8")
  !grepl("No records matched", tt)
}

#' @export
#' @rdname get_natservid
get_natservid_ <- function(query, messages = TRUE, rows = NA, key = NULL, ...) {
  stats::setNames(
    lapply(query, get_natservid_help, messages = messages, rows = rows,
           key = key, ...),
    query
  )
}

get_natservid_help <- function(query, messages, rows, key, ...) {
  mssg(messages, "\nRetrieving data for taxon '", query, "'\n")
  df <- ns_worker(query, key, ...)
  sub_rows(df, rows)
}

ns_base_uri <- function() "http://explorer.natureserve.org/servlet/NatureServe?searchSpeciesUid=%s"

ns_worker <- function(x, key, ...) {
  tmp <- tryCatch(natserv::ns_search(x = x, key = key, ...), error = function(e) e)
  if (inherits(tmp, "error")) return(tibble::tibble())
  tmp <- tmp[, c("globalSpeciesUid","jurisdictionScientificName","commonName","natureServeExplorerURI")]
  names(tmp) <- c('id', 'scientificname', 'commonname', 'uri')
  tmp
}
