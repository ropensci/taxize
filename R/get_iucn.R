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
#' IUCN ids only work with [synonyms()] and [sci2comm()]
#' methods.
#'
#' @family taxonomic-ids
#'
#' @examples \dontrun{
#' get_iucn("Branta canadensis")
#' get_iucn("Branta bernicla")
#' get_iucn("Panthera uncia")
#'
#' }
get_iucn <- function(sci, messages = TRUE, key = NULL, x = NULL, ...) {

  assert(sci, c("character", "taxon_state"))
  assert(messages, "logical")
  if (!is.null(x)) {
    lifecycle::deprecate_warn(when = "v0.9.97", what = "get_iucn(x)", with = "get_iucn(sci)")
    sci <- x
  }
  
  raw_data <- get_iucn_data(sci = sci, messages = messages, key = key, ...)
  
  out <- lapply(names(raw_data), function(input_name) {
    result <- raw_data[[input_name]]
    if (length(result) == 1 && is.na(result)) {
      id <- NA_character_
      att <- "not found"
    } else {
      id <- result$taxon$sis_id
      direct <- tolower(result$taxon$scientific_name) == tolower(input_name)
      att <- "found"
    }
    list(id = id, name = input_name, att = att, direct = direct)
  })

  ids <- structure(as.character(unlist(pluck(out, "id"))), class = "iucn",
                   match = pluck_un(out, "att", ""),
                   name = pluck_un(out, "name", ""))
  add_uri(ids, get_url_templates$iucn)
}


#' Get a IUCN Redlist taxon data
#' 
#' Used to get IUCN data for other functions to use.
#' 
#' @param If `TRUE`, latest use [rredlist::rl_species_latest()] instead of [rredlist::rl_species()]
#' 
#' @keywords internal
get_iucn_data <- function(sci, messages = TRUE, key = NULL, latest = FALSE, ...) {
  
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
  
  if (latest) {
    rl_func_to_use <- rredlist::rl_species_latest
  } else {
    rl_func_to_use <- rredlist::rl_species
  }
  
  for (i in seq_along(sci)) {
    direct <- FALSE
    mssg(messages, "\nRetrieving data for taxon '", sci[i], "'\n")
    parts <- strsplit(sci[i], split = ' +')[[1]]
    result <- tryCatch(
      {
        if (length(parts) == 2) {
          tmp <- rl_func_to_use(genus = parts[1], species = parts[2], key = key, ...)
        } else {
          tmp <- rl_func_to_use(genus = parts[1], species = parts[2], infra = parts[3], key = key, ...)
        }
        tmp
      },
      error = function(e) {
        NA_integer_
      }
    ) 
    
    if (length(result) == 1 && is.na(result)) {
      att <- "not found"
    } else {
      att <- "found"
    }
    prog$completed(sci[i], att)
    prog$prog(att)
    tstate$add(sci[i], result)
  }
  out <- tstate$get()
  on.exit(prog$prog_summary(), add = TRUE)
  on.exit(tstate$exit, add = TRUE)
  return(out)
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
