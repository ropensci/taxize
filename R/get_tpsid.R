#' Get the NameID codes from Tropicos for taxonomic names.
#'
#' @export
#' @param sci (character) One or more scientific name's as a vector or list. Or,
#' a `taxon_state` object (see [taxon-state])
#' @param ask logical; should get_tps be run in interactive mode?
#' If TRUE and more than one ID is found for the species, the user is asked for
#' input. If FALSE NA is returned for multiple matches.
#' @param messages logical; If TRUE the actual taxon queried is printed on the console.
#' @param key Your API key; see [taxize-authentication]
#' @param rows numeric; Any number from 1 to infinity. If the default NA, all rows are considered.
#' Note that this function still only gives back a tps class object with one to many identifiers.
#' See [get_tps_()] to get back all, or a subset, of the raw data that you are
#' presented during the ask process.
#' @param family (character) A family name. Optional. See `Filtering` below.
#' @param rank (character) A taxonomic rank name. See [rank_ref] for possible
#' options. Though note that some data sources use atypical ranks, so inspect the
#' data itself for options. Optional. See `Filtering` below.
#' @param sciname Deprecated, see `sci`
#' @param ... Other arguments passed to [tp_search()].
#' @param x Input to [as_tps()]
#' @param check logical; Check if ID matches any existing on the DB, only used in
#' [as_tps()]
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
#' @author Scott Chamberlain
#' @examples \dontrun{
#' get_tps(sci='Poa annua')
#' get_tps(sci='Pinus contorta')
#'
#' get_tps(c("Poa annua", "Pinus contorta"))
#'
#' # specify rows to limit choices available
#' get_tps('Poa ann')
#' get_tps('Poa ann', rows=1)
#' get_tps('Poa ann', rows=25)
#' get_tps('Poa ann', rows=1:2)
#'
#' # When not found, NA given (howdy is not a species name, and Chrinomus is a fly)
#' get_tps("howdy")
#' get_tps(c("Chironomus riparius", "howdy"))
#'
#' # Narrow down results to a division or rank, or both
#' ## Satyrium example
#' ### Results w/o narrowing
#' get_tps("Satyrium")
#' ### w/ rank
#' get_tps("Satyrium", rank = "var.")
#' get_tps("Satyrium", rank = "sp.")
#'
#' ## w/ family
#' get_tps("Poa")
#' get_tps("Poa", family = "Iridaceae")
#' get_tps("Poa", family = "Orchidaceae")
#' get_tps("Poa", family = "Orchidaceae", rank = "gen.")
#'
#' # Fuzzy filter on any filtering fields
#' ## uses grep on the inside
#' get_tps("Poa", family = "orchidaceae")
#' get_tps("Aga", fuzzy = TRUE, parent = "*idae")
#'
#' # pass to classification function to get a taxonomic hierarchy
#' classification(get_tps(sci='Poa annua'))
#'
#' # Convert a tps without class information to a tps class
#' as_tps(get_tps("Pinus contorta")) # already a tps, returns the same
#' as_tps(get_tps(c("Chironomus riparius","Pinus contorta"))) # same
#' as_tps(24900183) # numeric
#' as_tps(c(24900183,50150089,50079838)) # numeric vector, length > 1
#' as_tps("24900183") # character
#' as_tps(c("24900183","50150089","50079838")) # character vector, length > 1
#' as_tps(list("24900183","50150089","50079838")) # list, either numeric or character
#' ## dont check, much faster
#' as_tps("24900183", check=FALSE)
#' as_tps(24900183, check=FALSE)
#' as_tps(c("24900183","50150089","50079838"), check=FALSE)
#' as_tps(list("24900183","50150089","50079838"), check=FALSE)
#'
#' (out <- as_tps(c(24900183,50150089,50079838)))
#' data.frame(out)
#' as_tps( data.frame(out) )
#'
#' # Get all data back
#' get_tps_("Poa annua")
#' get_tps_("Poa annua", rows=2)
#' get_tps_("Poa annua", rows=1:2)
#' get_tps_(c("asdfadfasd","Pinus contorta"), rows=1:5)
#'
#' # use curl options
#' invisible(get_tps("Quercus douglasii", messages = TRUE))
#' }

get_tps <- function(sci, ask = TRUE, messages = TRUE, key = NULL,
  rows = NA, family = NULL, rank = NULL, sciname = NULL, ...) {

  assert(sci, c("character", "taxon_state"))
  assert(ask, "logical")
  assert(messages, "logical")
  assert(family, "character")
  assert(rank, "character")
  assert_rows(rows)
  pchk(sciname, "sci")

  if (inherits(sci, "character")) {
    tstate <- taxon_state$new(class = "tps", names = sci)
    items <- sci
  } else {
    assert_state(sci, "tps")
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
    id <- NA_character_
    name <- NA_character_
    rank_taken <- NA_character_
    mssg(messages, "\nRetrieving data for taxon '", sci[i], "'\n")
    tmp <- tp_search(sci = sci[i], key = key, ...)
    mm <- NROW(tmp) > 1

    if (
      all(names(tmp)[[1]] == "error") ||
      all(is.na(tmp)) ||
      inherits(tmp, "character") ||
      as.character(tmp) == "No names were found"
    ) {
      mssg(messages, m_not_found_sp_altclass)
      att <- "not found"
    } else {
      df <- tmp[, c('NameId','ScientificName','Family','RankAbbreviation',
                   'NomenclatureStatusName','Author','DisplayDate')]
      names(df) <- c('tpsid','name','family','rank','status','author','date')
      id <- df$tpsid
      att <- "found"
    }

    # not found on tropicos
    if (length(id) == 0) {
      mssg(messages, m_not_found_sp_altclass)
      att <- "not found"
    }
    # found on tropicos, length 1
    if (length(id) == 1 && att == "found") {
      att <- "found"
      direct <- TRUE
      rank_taken <- rank_swap(df$rank)
      name <- df$name
    }
    # more than one found on tropicos -> user input
    if (length(id) > 1) {

        if (!is.null(family) || !is.null(rank)) {
          df <- filt(df, "family", family)
          df <- filt(df, "rank", rank)
        }

        df$rank <- rank_swap(df$rank)

        df <- sub_rows(df, rows)
        id <- df$tpsid
        if (length(id) == 1) {
          rank_taken <- as.character(df$rank)
          name <- df$name
          direct <- TRUE
          att <- "found"
        }

        # more than one, try for direct match
        if (length(id) > 1) {
          matchtmp <- df[tolower(df$name) %in% tolower(sci[i]), ]
          if (NROW(matchtmp) == 1) {
            id <- matchtmp$tpsid
            rank_taken <- as.character(matchtmp$rank)
            name <- matchtmp$name
            direct <- TRUE
            att <- "found"
          }
        }

        if (length(id) > 1) {
          if (ask) {
            # prompt
            rownames(df) <- 1:nrow(df)
            message("\n\n")
            message("\nMore than one tps ID found for taxon '", sci[i], "'!\n
          Enter rownumber of taxon (other inputs will return 'NA'):\n")
            rownames(df) <- 1:nrow(df)
            print(df)
            take <- scan(n = 1, quiet = TRUE, what = 'raw')

            if (length(take) == 0) {
              take <- 'notake'
              att <- 'nothing chosen'
            }
            if (take %in% seq_len(nrow(df))) {
              take <- as.numeric(take)
              message("Input accepted, took tps ID '", as.character(df$tpsid[take]), "'.\n")
              id <- as.character(df$tpsid[take])
              rank_taken <- as.character(df$rank[take])
              name <- df$name[take]
              att <- "found"
            } else {
              mssg(messages, "\nReturned 'NA'!\n\n")
              att <- "not found"
            }
          } else {
            if (length(id) != 1) {
              warning(sprintf(m_more_than_one_found, "tps ID", sci[i]),
                call. = FALSE)
              att <- m_na_ask_false
            }
          }
        }
    }
    res <- list(id = as.character(id), name = name, rank = rank_taken,
      att = att, multiple = mm, direct = direct)
    prog$completed(sci[i], att)
    prog$prog(att)
    tstate$add(sci[i], res)
  }
  out <- tstate$get()
  res <- make_taxa_taxon(out, "tps")
  on.exit(prog$prog_summary(), add = TRUE)
  on.exit(tstate$exit, add = TRUE)
  return(res)
}
#' @export
#' @rdname get_tps
get_tpsid <- function(...) {
  fchk("get_tpsid", "get_tps")
  get_tps(...)
}

#' @export
#' @rdname get_tps
as_tps <- function(x, check=TRUE) UseMethod("as_tps")

#' @export
#' @rdname get_tps
as_tps.tps <- function(x, check=TRUE) x

#' @export
#' @rdname get_tps
as_tps.character <- function(x, check=TRUE) if(length(x) == 1) make_tps(x, check) else collapse(x, make_tps, "tps", check=check)

#' @export
#' @rdname get_tps
as_tps.list <- function(x, check=TRUE) if(length(x) == 1) make_tps(x, check) else collapse(x, make_tps, "tps", check=check)

#' @export
#' @rdname get_tps
as_tps.numeric <- function(x, check=TRUE) as_tps(as.character(x), check)

#' @export
#' @rdname get_tps
as_tps.data.frame <- function(x, check=TRUE) as_txid_df(x, check)

make_tps <- function(x, check=TRUE) make_generic(x, 'http://tropicos.org/Name/%s', "tps", check)

check_tps <- function(x){
  res <- tp_summary(x)
  !identical(names(res), "error")
}

#' @export
#' @rdname get_tps
get_tps_ <- function(sci, messages = TRUE, key = NULL, rows = NA,
  sciname = NULL, ...) {

  pchk(sciname, "sci")
  stats::setNames(lapply(sci, get_tps_help, messages = messages, key=key,
    rows = rows, ...), sci)
}
#' @export
#' @rdname get_tps
get_tpsid_ <- function(...) {
  fchk("get_tpsid_", "get_tps_")
  get_tps_(...)
}

get_tps_help <- function(sci, messages, key, rows, ...){
  mssg(messages, "\nRetrieving data for taxon '", sci, "'\n")
  df <- tp_search(sci=sci, key=key, ...)
  if("error" %in% names(df)) NULL else sub_rows(df, rows)
}
