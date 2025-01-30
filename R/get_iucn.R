iucn_base_url <- "https://www.iucnredlist.org/details/%s/0"


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

  assert(sci, c("character", "taxon_state", "numeric"))
  assert(messages, "logical")
  if (!is.null(x)) {
    lifecycle::deprecate_warn(when = "v0.9.97", what = "get_iucn(x)", with = "get_iucn(sci)")
    sci <- x
  }
  
  raw_data <- get_iucn_data(names_or_ids = sci, messages = messages, key = key, ...)
  
  out <- lapply(names(raw_data), function(input_name) {
    result <- raw_data[[input_name]]
    if (length(result) == 1 && is.na(result)) {
      id <- NA_character_
      direct <- NA
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
#' @param latest If `TRUE`, latest use [rredlist::rl_species_latest()] instead of [rredlist::rl_species()]
#' 
#' @keywords internal
get_iucn_data <- function(names_or_ids, messages = TRUE, key = NULL, latest = FALSE, ...) {
  
  if (inherits(names_or_ids, "character") || inherits(names_or_ids, "numeric")) {
    tstate <- taxon_state$new(class = "iucn", names = names_or_ids)
    items <- names_or_ids
  } else {
    assert_state(names_or_ids, "iucn")
    tstate <- names_or_ids
    names_or_ids <- tstate$taxa_remaining()
    items <- c(names_or_ids, tstate$taxa_completed())
  }
  
  prog <- progressor$new(items = items, suppress = !messages)
  done <- tstate$get()
  for (i in seq_along(done)) prog$completed(names(done)[i], done[[i]]$att)
  prog$prog_start()
  
  
  for (i in seq_along(names_or_ids)) {
    direct <- FALSE
    mssg(messages, "\nRetrieving data for taxon '", names_or_ids[i], "'\n")
    result <- tryCatch(
      {
        if (is.numeric(names_or_ids[i]) || grepl(names_or_ids[i], pattern = '^[0-9]+$')) {
          if (latest) {
            tmp <- rredlist::rl_sis_latest(names_or_ids[i], key = key, ...)
          } else {
            tmp <- rredlist::rl_sis(as.numeric(names_or_ids[i]), key = key, ...)
          }
        } else {
          parts <- strsplit(names_or_ids[i], split = ' +')[[1]]
          if (latest) {
            rl_func_to_use <- rredlist::rl_species_latest
          } else {
            rl_func_to_use <- rredlist::rl_species
          }
          if (length(parts) == 2) {
            tmp <- rl_func_to_use(genus = parts[1], species = parts[2], key = key, ...)
          } else {
            tmp <- rl_func_to_use(genus = parts[1], species = parts[2], infra = parts[3], key = key, ...)
          }
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
    prog$completed(names_or_ids[i], att)
    prog$prog(att)
    tstate$add(as.character(names_or_ids[i]), result)
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
  make_iucn(x, check = check, key = key)
}

#' @export
#' @rdname get_iucn
as.iucn.list <- function(x, check = TRUE, key = NULL) {
  as.list(make_iucn(unlist(x), check = check, key = key))
}

#' @export
#' @rdname get_iucn
as.iucn.numeric <- function(x, check=TRUE, key = NULL) {
  make_iucn(x, check = check, key = key)
}

#' @export
#' @rdname get_iucn
as.iucn.data.frame <- function(x, check=TRUE, key = NULL) {
  make_iucn(x$ids, check = check, key = key)
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

#' @keywords internal
make_iucn <- function(x, uu = iucn_base_url, clz = "iucn", check = TRUE, key = NULL) {
  if (check) {
    res <- get_iucn(x, key = key)
  } else {
    toid(x, uu, clz)
  }
}
