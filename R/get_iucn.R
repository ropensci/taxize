#' Get a IUCN Redlist taxon
#'
#' @export
#' @param x (character) A vector of common or scientific names. Or, a
#' [taxon_state()] object
#' @param messages logical; should progress be printed?
#' @param key (character) required. you IUCN Redlist API key. See
#' [`rredlist::rredlist-package`] for help on authenticating with
#' IUCN Redlist
#' @param check (logical) Check if ID matches any existing on the DB, only
#' used in [`as.iucn()`]
#' @param ... Ignored
#'
#' @return A vector of taxonomic identifiers as an S3 class.
#'
#' Comes with the following attributes:
#' * *match* (character) - the reason for NA, either 'not found',
#'  'found' or if `ask = FALSE` then 'NA due to ask=FALSE')
#' * *name* (character) - the taxonomic name, which is needed in
#'  [`synonyms()`] and [`sci2comm()`] methods since they
#'  internally use \pkg{rredlist} functions which require the taxonomic name,
#'  and not the taxonomic identifier
#' * *ri* (character) - The URI where more information can be
#'  read on the taxon - includes the taxonomic identifier in the URL somewhere
#'
#' *multiple_matches* and *pattern_match* do not apply here as in other `get_*`
#' methods since there is no IUCN Redlist search, so you either get a match or
#' you do not get a match.
#'
#' @details There is no underscore method, because there's no real
#' search for IUCN, that is, where you search for a string, and get back
#' a bunch of results due to fuzzy matching. If that exists in the future
#' we'll add an underscore method here.
#'
#' IUCN ids only work with [`synonyms()`] and [`sci2comm()`]
#' methods.
#'
#' @family taxonomic-ids
#'
#' @examples \dontrun{
#' get_iucn(x = "Branta canadensis")
#' get_iucn(x = "Branta bernicla")
#' get_iucn(x = "Panthera uncia")
#'
#' # as coercion
#' as.iucn(22732)
#' as.iucn("22732")
#' (res <- as.iucn(c(22679946, 22732, 22679935)))
#' data.frame(res)
#' as.iucn(data.frame(res))
#' }
get_iucn <- function(x, messages = TRUE, key = NULL, ...) {

  assert(x, c("character", "taxon_state"))
  assert(messages, "logical")

  if (inherits(x, "character")) {
    tstate <- taxon_state$new(class = "iucn", names = x)
    items <- x
  } else {
    tstate <- x
    x <- tstate$taxa_remaining()
    items <- c(x, tstate$taxa_completed())
  }

  prog <- progressor$new(items = items, suppress = !messages)
  done <- tstate$get()
  for (i in seq_along(done)) prog$completed(names(done)[i], done[[i]]$att)
  prog$prog_start()

  for (i in seq_along(x)) {
    direct <- FALSE
    mssg(messages, "\nRetrieving data for taxon '", x[i], "'\n")
    df <- rredlist::rl_search(x[i], key = key, ...)

    if (!inherits(df$result, "data.frame") || NROW(df$result) == 0) {
      id <- NA_character_
      att <- "not found"
    } else {
      df <- df$result[, c("taxonid", "scientific_name", "kingdom",
                   "phylum", "order", "family", "genus", "authority")]

      # should return NA if species not found
      if (NROW(df) == 0) {
        mssg(messages, tx_msg_not_found)
        id <- NA_character_
        att <- "not found"
      }

      # check for direct match
      direct <- match(tolower(df$scientific_name), tolower(x[i]))

      if (!all(is.na(direct))) {
        id <- df$taxonid[!is.na(direct)]
        direct <- TRUE
        att <- "found"
      } else {
        direct <- FALSE
        id <- df$taxonid
        att <- "found"
      }
      # multiple matches not possible because no real search
    }
    res <- list(id = id, name = x[i], att = att, direct = direct)
    prog$completed(x[i], att)
    prog$prog(att)
    tstate$add(x[i], res)
  }
  out <- tstate$get()
  ids <- structure(as.character(unlist(pluck(out, "id"))), class = "iucn",
                   match = pluck_un(out, "att", ""),
                   name = pluck_un(out, "name", ""))
  on.exit(prog$prog_summary(), add = TRUE)
  on.exit(tstate$exit, add = TRUE)
  add_uri(ids, get_url_templates$iucn)
}

#' @export
#' @rdname get_iucn
as.iucn <- function(x, check = TRUE, key = NULL) {
  UseMethod("as.iucn")
}

#' @export
#' @rdname get_iucn
as.iucn.iucn <- function(x, check = TRUE, key = NULL) x

#' @export
#' @rdname get_iucn
as.iucn.character <- function(x, check = TRUE, key = NULL) {
  if (length(x) == 1) {
    make_iucn(x, check, key = key)
  } else {
    collapse(x, make_iucn, "iucn", check = check, key = key)
  }
}

#' @export
#' @rdname get_iucn
as.iucn.list <- function(x, check = TRUE, key = NULL) {
  if (length(x) == 1) {
    make_iucn(x, check)
  } else {
    collapse(x, make_iucn, "iucn", check = check)
  }
}

#' @export
#' @rdname get_iucn
as.iucn.numeric <- function(x, check=TRUE, key = NULL) {
  as.iucn(as.character(x), check, key = key)
}

#' @export
#' @rdname get_iucn
as.iucn.data.frame <- function(x, check=TRUE, key = NULL) {
  structure(x$ids, class = "iucn", match = x$match,
            name = x$name, uri = x$uri)
}

#' @export
#' @rdname get_iucn
as.data.frame.iucn <- function(x, ...){
  data.frame(ids = unclass(x),
             class = "iucn",
             name = attr(x, "name"),
             match = attr(x, "match"),
             uri = attr(x, "uri"),
             stringsAsFactors = FALSE)
}

make_iucn <- function(x, check = TRUE, key = NULL) {
  make_iucn_generic(x, uu = iucn_base_url, clz = "iucn", check, key)
}

check_iucn <- function(x) {
  cli <- crul::HttpClient$new(sprintf(iucn_base_url, x), headers = tx_ual)
  tt <- cli$get()
  tt$status_code == 200
}

check_iucn_getname <- function(x, key = NULL) {
  rredlist::rl_search(id = as.numeric(x), key = key)
}

iucn_base_url <- "https://www.iucnredlist.org/details/%s/0"
