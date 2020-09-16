#' Get the OTT id for a search term
#'
#' Retrieve the Open Tree of Life Taxonomy (OTT) id of a taxon from
#' OpenTreeOfLife
#'
#' @export
#' @param sci character; one or more scientific names. Or, a `taxon_state`
#' object (see [taxon-state])
#' @param ask logical; should `get_tol` be run in interactive mode?
#' If `TRUE` and more than one TOL is found for the species, the user is
#' asked for input. If `FALSE` NA is returned for multiple matches.
#' @param messages logical; should progress be printed?
#' @param rows numeric; Any number from 1 to infinity. If the default NA, all
#' rows are considered. Note that this function still only gives back a tol
#' class object with one to many identifiers. See [get_tol_()]
#' to get back all, or a subset, of the raw data that you are presented during
#' the ask process.
#' @param x Input to `as.tol`
#' @param sciname Deprecated, see `sci`
#' @param ... Ignored
#' @param check logical; Check if ID matches any existing on the DB, only
#' used in [as.tol()]
#' @template getreturn
#'
#' @family taxonomic-ids
#' @seealso [classification()]
#'
#' @examples \dontrun{
#' get_tol(sci = "Quercus douglasii")
#' get_tol(sci = "Chironomus riparius")
#' get_tol(c("Chironomus riparius","Quercus douglasii"))
#' splist <- c("annona cherimola", 'annona muricata', "quercus robur",
#' 		"shorea robusta", "pandanus patina", "oryza sativa", "durio zibethinus")
#' get_tol(splist, messages=FALSE)
#'
#' # specify rows to limit choices available
#' get_tol('Arni')
#' get_tol('Arni', rows=1)
#' get_tol('Arni', rows=1:2)
#'
#' # When not found
#' get_tol("howdy")
#' get_tol(c("Chironomus riparius", "howdy"))
#'
#' # Convert a tol without class information to a tol class
#' as.tol(get_tol("Quercus douglasii")) # already a tol, returns the same
#' as.tol(get_tol(c("Chironomus riparius","Pinus contorta"))) # same
#' as.tol(5907893) # numeric
#' as.tol(c(3930798,515712,872577)) # numeric vector, length > 1
#' as.tol("3930798") # character
#' as.tol(c("3930798","515712","872577")) # character vector, length > 1
#' as.tol(list("3930798","515712","872577")) # list, either numeric or character
#' ## dont check, much faster
#' as.tol("3930798", check=FALSE)
#' as.tol(3930798, check=FALSE)
#' as.tol(c("3930798","515712","872577"), check=FALSE)
#' as.tol(list("3930798","515712","872577"), check=FALSE)
#'
#' (out <- as.tol(c(3930798,515712,872577)))
#' data.frame(out)
#' as.tol( data.frame(out) )
#'
#' # Get all data back
#' get_tol_("Arni")
#' get_tol_("Arni", rows=1)
#' get_tol_("Arni", rows=1:2)
#' get_tol_(c("asdfadfasd","Pinus contorta"))
#' }

get_tol <- function(sci, ask = TRUE, messages = TRUE, rows = NA,
  sciname = NULL, ...) {

  assert(sci, c("character", "taxon_state"))
  assert(ask, "logical")
  assert(messages, "logical")
  pchk(sciname, "sci")
  if (!all(is.na(rows))) {
    assert(rows, c("numeric", "integer"))
    stopifnot(rows > 0)
  }

  if (inherits(sci, "character")) {
    tstate <- taxon_state$new(class = "tol", names = sci)
    items <- sci
  } else {
    assert_state(sci, "tol")
    tstate <- sci
    sci <- tstate$taxa_remaining()
    items <- c(sci, tstate$taxa_completed())
  }

  prog <- progressor$new(items = items, suppress = !messages)
  done <- tstate$get()
  for (i in seq_along(done)) prog$completed(names(done)[i], done[[i]]$att)
  prog$prog_start()

  for (i in seq_along(sci)) {
    rank_taken <- NA_character_
    name <- NA_character_
    mssg(messages, "\nRetrieving data for taxon '", sci[i], "'\n")

    tol_df <- tryCatch(tol_resolve(sci[i], ...), error = function(e) e)

    if (
      !inherits(tol_df, "data.frame") ||
      NROW(tol_df) == 0 ||
      inherits(tol_df, "error")
    ) {
      id <- NA_character_
      att <- "not found"
    } else {
      tol_df <- tol_fetch_fuzzy(tol_df)
      tol_df <- sub_rows(tol_df, rows)

      # should return NA if spec not found
      if (NROW(tol_df) == 0) {
        mssg(messages, m_not_found_sp_altclass)
        id <- NA_character_
        att <- 'not found'
      }

      # take the one tol from data.frame
      if (NROW(tol_df) == 1) {
        id <- tol_df$ott_id
        rank_taken <- tol_df$rank
        name <- tol_df$matched_name
        att <- 'found'
      }

      # more than one found -> user input
      if (NROW(tol_df) > 1) {
        # check for exact match
        matchtmp <- tol_df[tolower(tol_df$matched_name) %in% tolower(sci[i]), ]
        if (length(matchtmp) == 1) {
          id <- as.character(matchtmp$ott_id)
          rank_taken <- matchtmp$rank
          name <- matchtmp$matched_name
          direct <- TRUE
          att <- "found"
        } else {
          tol_df <- sub_rows(tol_df, rows)
          if (NROW(tol_df) == 0) {
            id <- NA_character_
            att <- "not found"
          } else {
            id <- tol_df$ott_id
            if (length(id) == 1) {
              rank_taken <- tol_df$rank
              name <- tol_df$matched_name
              att <- "found"
            }
          }

          if (length(id) > 1) {
            if (ask) {
              # prompt
              message("\n\n")
              message("\nMore than one ToL ID found for taxon '", sci[i], "'!\n
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
                message("Input accepted, took tol ID '",
                  as.character(tol_df$ott_id[take]), "'.\n")
                id <- as.character(tol_df$ott_id[take])
                rank_taken <- tol_df$rank[take]
                name <- tol_df$matched_name[take]
                att <- "found"
              } else {
                id <- NA_character_
                att <- "not found"
                mssg(messages, "\nReturned 'NA'!\n\n")
              }
            }
            else {
              if (length(id) != 1) {
                warning(sprintf(m_more_than_one_found, "ToL ID", sci[i]),
                  call. = FALSE)
                id <- NA_character_
                att <- m_na_ask_false
              }
            }
          }
        }
      }
    }
    res <- list(id = as.character(id), name = name, rank = rank_taken,
      att = att, multiple = FALSE, direct = FALSE)
    prog$completed(sci[i], att)
    prog$prog(att)
    tstate$add(sci[i], res)
  }
  out <- tstate$get()
  ids <- as.character(unlist(pluck(out, "id")))
  res <- taxa_taxon(
    name = unlist(pluck(out, "name")),
    id = taxa::taxon_id(ids, db = "tol"),
    rank = unlist(pluck(out, "rank")),
    uri = sprintf(get_url_templates$tol, ids),
    match = unname(unlist(pluck(out, "att"))),
    multiple_matches = unname(unlist(pluck(out, "multiple"))),
    pattern_match = unname(unlist(pluck(out, "direct"))),
    class = "tol"
  )
  on.exit(prog$prog_summary(), add = TRUE)
  on.exit(tstate$exit, add = TRUE)
  return(res)
}
#' @export
#' @rdname get_tol
get_tolid <- get_tol

#' @export
#' @rdname get_tol
as.tol <- function(x, check=TRUE) UseMethod("as.tol")

#' @export
#' @rdname get_tol
as.tol.tol <- function(x, check=TRUE) x

#' @export
#' @rdname get_tol
as.tol.character <- function(x, check=TRUE) {
  if (length(x) == 1) make_tol(x, check) else collapse(x, make_tol, "tol", check = check)
}

#' @export
#' @rdname get_tol
as.tol.list <- function(x, check=TRUE) {
  if (length(x) == 1) make_tol(x, check) else collapse(x, make_tol, "tol", check = check)
}

#' @export
#' @rdname get_tol
as.tol.numeric <- function(x, check=TRUE) as.tol(as.character(x), check)

#' @export
#' @rdname get_tol
as.tol.data.frame <- function(x, check=TRUE) as_txid_df(x, check)

make_tol <- function(x, check=TRUE) {
  make_generic(x, 'https://tree.opentreeoflife.org/opentree/argus/ottol@%s', "tol", check)
}

check_tol <- function(x){
  tt <- tryCatch(rotl::taxonomy_taxon_info(as.numeric(x)), error = function(e) e)
  !inherits(tt, "error")
}

#' @export
#' @rdname get_tol
get_tol_ <- function(sci, messages = TRUE, rows = NA, sciname = NULL) {
  pchk(sciname, "sci")
  stats::setNames(
    lapply(sci, get_tol_help, messages = messages, rows = rows),
    sci
  )
}
#' @export
#' @rdname get_tol
get_tolid_ <- get_tol_

get_tol_help <- function(sci, messages, rows, ...){
  mssg(messages, "\nRetrieving data for taxon '", sci, "'\n")
  tol_df <- tryCatch(tol_resolve(sci, ...), error = function(e) e)
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
  df$tax_sources <- NULL
  df$synonyms <- NULL
  df$is_suppressed <- NULL
  df$is_suppressed_from_synth <- NULL
  df$search_string <- NULL
  df$name <- NULL
  df$source <- NULL
  df
}

dtrbsetdf <- function(x) {
  x <- lapply(x, function(a) {
    a[sapply(a, is.null)] <- NA_character_
    a
  })
  (out <- data.table::setDF(
    data.table::rbindlist(x, use.names = TRUE, fill = TRUE)
  ))
}
