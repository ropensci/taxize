#' Get the NameID codes from Tropicos for taxonomic names.
#'
#' @export
#' @param sci (character) One or more scientific name's as a vector or list. Or,
#' a `taxon_state` object (see [taxon-state])
#' @param ask logical; should get_tpsid be run in interactive mode?
#' If TRUE and more than one ID is found for the species, the user is asked for
#' input. If FALSE NA is returned for multiple matches.
#' @param messages logical; If TRUE the actual taxon queried is printed on the console.
#' @param key Your API key; see [taxize-authentication]
#' @param rows numeric; Any number from 1 to infinity. If the default NA, all rows are considered.
#' Note that this function still only gives back a tpsid class object with one to many identifiers.
#' See [get_tpsid_()] to get back all, or a subset, of the raw data that you are
#' presented during the ask process.
#' @param family (character) A family name. Optional. See `Filtering` below.
#' @param rank (character) A taxonomic rank name. See [rank_ref] for possible
#' options. Though note that some data sources use atypical ranks, so inspect the
#' data itself for options. Optional. See `Filtering` below.
#' @param sciname Deprecated, see `sci`
#' @param ... Other arguments passed to [tp_search()].
#' @param x Input to [as.tpsid()]
#' @param check logical; Check if ID matches any existing on the DB, only used in
#' [as.tpsid()]
#' @template getreturn
#'
#' @section Filtering:
#' The parameters `family` an`rank`nk are not used in the search to the data
#' provider, but are used in filtering the data down to a subset that is closer to the
#' target you want.  For all these parameters,
#' you can use regex strings since we use [grep()] internally to match.
#' Filtering narrows down to the set that matches your query, and removes the rest.
#'
#' @family taxonomic-ids
#' @seealso [classification()]
#'
#' @author Scott Chamberlain, 
#'
#' @examples \dontrun{
#' get_tpsid(sci='Poa annua')
#' get_tpsid(sci='Pinus contorta')
#'
#' get_tpsid(c("Poa annua", "Pinus contorta"))
#'
#' # specify rows to limit choices available
#' get_tpsid('Poa ann')
#' get_tpsid('Poa ann', rows=1)
#' get_tpsid('Poa ann', rows=25)
#' get_tpsid('Poa ann', rows=1:2)
#'
#' # When not found, NA given (howdy is not a species name, and Chrinomus is a fly)
#' get_tpsid("howdy")
#' get_tpsid(c("Chironomus riparius", "howdy"))
#'
#' # Narrow down results to a division or rank, or both
#' ## Satyrium example
#' ### Results w/o narrowing
#' get_tpsid("Satyrium")
#' ### w/ rank
#' get_tpsid("Satyrium", rank = "var.")
#' get_tpsid("Satyrium", rank = "sp.")
#'
#' ## w/ family
#' get_tpsid("Poa")
#' get_tpsid("Poa", family = "Iridaceae")
#' get_tpsid("Poa", family = "Orchidaceae")
#' get_tpsid("Poa", family = "Orchidaceae", rank = "gen.")
#'
#' # Fuzzy filter on any filtering fields
#' ## uses grep on the inside
#' get_tpsid("Poa", family = "orchidaceae")
#' get_tpsid("Aga", fuzzy = TRUE, parent = "*idae")
#'
#' # pass to classification function to get a taxonomic hierarchy
#' classification(get_tpsid(sci='Poa annua'))
#'
#' # Convert a tpsid without class information to a tpsid class
#' as.tpsid(get_tpsid("Pinus contorta")) # already a tpsid, returns the same
#' as.tpsid(get_tpsid(c("Chironomus riparius","Pinus contorta"))) # same
#' as.tpsid(24900183) # numeric
#' as.tpsid(c(24900183,50150089,50079838)) # numeric vector, length > 1
#' as.tpsid("24900183") # character
#' as.tpsid(c("24900183","50150089","50079838")) # character vector, length > 1
#' as.tpsid(list("24900183","50150089","50079838")) # list, either numeric or character
#' ## dont check, much faster
#' as.tpsid("24900183", check=FALSE)
#' as.tpsid(24900183, check=FALSE)
#' as.tpsid(c("24900183","50150089","50079838"), check=FALSE)
#' as.tpsid(list("24900183","50150089","50079838"), check=FALSE)
#'
#' (out <- as.tpsid(c(24900183,50150089,50079838)))
#' data.frame(out)
#' as.tpsid( data.frame(out) )
#'
#' # Get all data back
#' get_tpsid_("Poa annua")
#' get_tpsid_("Poa annua", rows=2)
#' get_tpsid_("Poa annua", rows=1:2)
#' get_tpsid_(c("asdfadfasd","Pinus contorta"), rows=1:5)
#'
#' # use curl options
#' invisible(get_tpsid("Quercus douglasii", messages = TRUE))
#' }

get_tpsid <- function(sci, ask = TRUE, messages = TRUE, key = NULL,
  rows = NA, family = NULL, rank = NULL, sciname = NULL, ...) {

  assert(sci, c("character", "taxon_state"))
  assert(ask, "logical")
  assert(family, "character")
  assert(rank, "character")
  assert_rows(rows)
  if (!is.null(sciname)) {
    lifecycle::deprecate_warn(when = "v0.9.97", what = "get_tpsid(sciname)", with = "get_tpsid(sci)")
    sci <- sciname
  }
  

  if (inherits(sci, "character")) {
    tstate <- taxon_state$new(class = "tpsid", names = sci)
    items <- sci
  } else {
    assert_state(sci, "tpsid")
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
    tmp <- tp_search(sci = sci[i], key = key, ...)
    mm <- NROW(tmp) > 1

    if (
      all(names(tmp)[[1]] == "error") ||
      all(is.na(tmp)) ||
      inherits(tmp, "character")
    ) {
      mssg(messages, m_not_found_sp_altclass)
      id <- NA_character_
      att <- "not found"
    } else {
      df <- tmp[, c('nameid','scientificname','family','rankabbreviation',
                   'nomenclaturestatusname','author','displaydate')]
      names(df) <- c('tpsid','name','family','rank','status','author','date')
      id <- df$tpsid
      att <- "found"
    }

    # not found on tropicos
    if (length(id) == 0) {
      mssg(messages, m_not_found_sp_altclass)
      id <- NA_character_
      att <- "not found"
    }
    # more than one found on tropicos -> user input
    if (length(id) > 1) {

        if (!is.null(family) || !is.null(rank)) {
          df <- filt(df, "family", family)
          df <- filt(df, "rank", rank)
        }

        df <- sub_rows(df, rows)
        id <- df$tpsid
        if (length(id) == 1) {
          rank_taken <- as.character(df$rank)
          direct <- TRUE
          att <- "found"
        }

        # more than one, try for direct match
        if (length(id) > 1) {
          matchtmp <- df[tolower(df$name) %in% tolower(sci[i]), "tpsid"]
          if (length(matchtmp) == 1) {
            id <- matchtmp
            direct <- TRUE
            att <- "found"
          }
        }

        if (length(id) > 1) {
          if (ask) {
            # prompt
            rownames(df) <- 1:nrow(df)
            message("\n\n")
            message("\nMore than one tpsid found for taxon '", sci[i], "'!\n
          Enter rownumber of taxon (other inputs will return 'NA'):\n")
            rownames(df) <- 1:nrow(df)
            message(paste0(utils::capture.output(df), collapse = "\n"))
            take <- scan(n = 1, quiet = TRUE, what = 'raw')

            if (length(take) == 0) {
              take <- 'notake'
              att <- 'nothing chosen'
            }
            if (take %in% seq_len(nrow(df))) {
              take <- as.numeric(take)
              message("Input accepted, took tpsid '", as.character(df$tpsid[take]), "'.\n")
              id <- as.character(df$tpsid[take])
              att <- "found"
            } else {
              id <- NA_character_
              mssg(messages, "\nReturned 'NA'!\n\n")
              att <- "not found"
            }
          } else {
            if (length(id) != 1) {
              warning(sprintf(m_more_than_one_found, "tpsid", sci[i]),
                call. = FALSE)
              id <- NA_character_
              att <- m_na_ask_false
            }
          }
        }
    }
    res <- list(id = as.character(id), att = att, multiple = mm, direct = direct)
    prog$completed(sci[i], att)
    prog$prog(att)
    tstate$add(sci[i], res)
  }
  out <- tstate$get()
  ids <- structure(as.character(unlist(pluck(out, "id"))), class = "tpsid",
                   match = pluck_un(out, "att", ""),
                   multiple_matches = pluck_un(out, "multiple", logical(1)),
                   pattern_match = pluck_un(out, "direct", logical(1)))
  on.exit(prog$prog_summary(), add = TRUE)
  on.exit(tstate$exit, add = TRUE)
  add_uri(ids, get_url_templates$tropicos)
}

#' @export
#' @rdname get_tpsid
as.tpsid <- function(x, check=TRUE) UseMethod("as.tpsid")

#' @export
#' @rdname get_tpsid
as.tpsid.tpsid <- function(x, check=TRUE) x

#' @export
#' @rdname get_tpsid
as.tpsid.character <- function(x, check=TRUE) if(length(x) == 1) make_tpsid(x, check) else collapse(x, make_tpsid, "tpsid", check=check)

#' @export
#' @rdname get_tpsid
as.tpsid.list <- function(x, check=TRUE) if(length(x) == 1) make_tpsid(x, check) else collapse(x, make_tpsid, "tpsid", check=check)

#' @export
#' @rdname get_tpsid
as.tpsid.numeric <- function(x, check=TRUE) as.tpsid(as.character(x), check)

#' @export
#' @rdname get_tpsid
as.tpsid.data.frame <- function(x, check=TRUE) {
  structure(x$ids, class="tpsid", match=x$match,
            multiple_matches = x$multiple_matches,
            pattern_match = x$pattern_match, uri=x$uri)
}

#' @export
#' @rdname get_tpsid
as.data.frame.tpsid <- function(x, ...){
  data.frame(ids = as.character(unclass(x)),
             class = "tpsid",
             match = attr(x, "match"),
             multiple_matches = attr(x, "multiple_matches"),
             pattern_match = attr(x, "pattern_match"),
             uri = attr(x, "uri"),
             stringsAsFactors = FALSE)
}

make_tpsid <- function(x, check=TRUE) make_generic(x, 'http://tropicos.org/Name/%s', "tpsid", check)

check_tpsid <- function(x){
  res <- tp_summary(x)
  !identical(names(res), "error")
}

#' @export
#' @rdname get_tpsid
get_tpsid_ <- function(sci, messages = TRUE, key = NULL, rows = NA,
  sciname = NULL, ...) {

  if (!is.null(sciname)) {
    lifecycle::deprecate_warn(when = "v0.9.97", what = "get_tpsid_(sciname)", with = "get_tpsid_(sci)")
    sci <- sciname
  }
  stats::setNames(lapply(sci, get_tpsid_help, messages = messages, key=key,
    rows = rows, ...), sci)
}

get_tpsid_help <- function(sci, messages, key, rows, ...){
  mssg(messages, "\nRetrieving data for taxon '", sci, "'\n")
  df <- tp_search(sci=sci, key=key, ...)
  if("error" %in% names(df)) NULL else sub_rows(df, rows)
}
