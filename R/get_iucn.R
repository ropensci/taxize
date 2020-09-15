#' Get a IUCN Redlist taxon
#'
#' @export
#' @param sci (character) A vector of scientific names. Or, a
#' `taxon_state` object (see [taxon-state])
#' @param messages logical; should progress be printed?
#' @param key (character) required. you IUCN Redlist API key. See
#' [rredlist::rredlist-package] for help on authenticating with
#' IUCN Redlist
#' @param check (logical) Check if ID matches any existing on the DB, only
#' used in [as.iucn()]
#' @param x For `get_iucn()`: Deprecated, see `sci`. For `as.iucn()`, various,
#' see examples
#' @param ... Ignored
#'
#' @return A vector of taxonomic identifiers as an S3 class.
#'
#' Comes with the following attributes:
#' 
#' * *match* (character) - the reason for NA, either 'not found',
#'  'found' or if `ask = FALSE` then 'NA due to ask=FALSE')
#' * *name* (character) - the taxonomic name, which is needed in
#'  [synonyms()] and [sci2comm()] methods since they
#'  internally use \pkg{rredlist} functions which require the taxonomic name,
#'  and not the taxonomic identifier
#' * *uri* (character) - The URI where more information can be
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
#' IUCN ids only work with [synonyms()] and [sci2comm()]
#' methods.
#' 
#' There's no concept of a taxon rank other than "species" for IUCN. i.e., 
#' all returned data from this function should be of rank "species"
#'
#' @family taxonomic-ids
#'
#' @examples \dontrun{
#' get_iucn("Branta canadensis")
#' get_iucn("Branta bernicla")
#' get_iucn("Panthera uncia")
#' get_iucn(c("Panthera uncia", "Branta bernicla"))
#'
#' # as coercion
#' as.iucn(22732)
#' as.iucn("22732")
#' (res <- as.iucn(c(22679946, 22732, 22679935)))
#' data.frame(res)
#' as.iucn(data.frame(res))
#' }
get_iucn <- function(sci, messages = TRUE, key = NULL, x = NULL, ...) {

  assert(sci, c("character", "taxon_state"))
  assert(messages, "logical")
  pchk(x, "sci")

  if (inherits(sci, "character")) {
    tstate <- taxon_state$new(class = "iucn", names = sci)
    items <- sci
  } else {
    assert_state(sci, "iucn")
    tstate <- sci
    sci <- tstate$taxa_remaining()
    items <- c(sci, tstate$taxa_completed())
  }

  prog <- progressor$new(items = items, suppress = !messages)
  done <- tstate$get()
  for (i in seq_along(done)) prog$completed(names(done)[i], done[[i]]$att)
  prog$prog_start()

  for (i in seq_along(sci)) {
    direct <- FALSE
    mssg(messages, "\nRetrieving data for taxon '", sci[i], "'\n")
    df <- rredlist::rl_search(sci[i], key = key, ...)

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
      direct <- match(tolower(df$scientific_name), tolower(sci[i]))

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
    res <- list(id = id, name = sci[i], att = att, direct = direct)
    prog$completed(sci[i], att)
    prog$prog(att)
    tstate$add(sci[i], res)
  }
  out <- tstate$get()
  ids <- as.character(unlist(pluck(out, "id")))
  res <- taxa_taxon(
    name = unlist(pluck(out, "name")),
    id = taxa::taxon_id(ids, db = "iucn"),
    rank = "species",
    uri = sprintf(get_url_templates$iucn, ids),
    match = unname(unlist(pluck(out, "att"))),
    class = "iucn"
  )
  on.exit(prog$prog_summary(), add = TRUE)
  on.exit(tstate$exit, add = TRUE)
  return(res)
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
