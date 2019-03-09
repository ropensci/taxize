#' Get rank for a given taxonomic name.
#'
#' @export
#' @param x (character) Vector of one or more taxon names (character) or
#' IDs (character or numeric) to query. Or objects returned from `get_*()`
#' functions like [`get_tsn()`]
#' @param db (character) database to query. either `ncbi`, `itis`, `eol`, `col`,
#' `tropicos`, `gbif`,`nbn`, `worms`, `natserv`, `bold`. Note that each
#' taxonomic data source has their own identifiers, so that if you provide the
#' wrong `db` value for the identifier you may get a result, but it will
#' likely be wrong (not what you were expecting). If using ncbi or eol we
#' recommend getting an API key; see [`taxize-authentication`]
#' @param rows numeric; Any number from 1 to infinity. If the default NA, 
#' all rows are considered. passed down to `get_*()` functions.
#' @param ... Additional arguments to [`classification()`]
#' @return A named list of character vectors with ranks (all lower-cased)
#' @note While [`tax_name()`] returns the name of a specified
#' rank, [`tax_rank()`] returns the actual rank of the taxon.
#' @seealso [`classification()`],[`tax_name()`]
#' @examples \dontrun{
#' tax_rank(x = "Helianthus annuus", db = "itis")
#' tax_rank(get_tsn("Helianthus annuus"))
#' tax_rank(c("Helianthus", "Pinus", "Poa"), db = "itis")
#'
#' tax_rank(get_boldid("Helianthus annuus"))
#' tax_rank("421377", db = "bold")
#' tax_rank(421377, db = "bold")
#'
#' tax_rank(c("Plantae", "Helianthus annuus",
#'   "Puma", "Homo sapiens"), db = 'itis')
#' tax_rank(c("Helianthus annuus", "Quercus", "Fabaceae"), db = 'tropicos')
#'
#' tax_rank(names_list("species"), db = 'gbif')
#' tax_rank(names_list("family"), db = 'gbif')
#'
#' tax_rank(c("Platanista gangetica", "Lichenopora neapolitana"),
#'   db = "worms")
#' }
tax_rank <- function(x, db = NULL, rows = NA, ...) {
  UseMethod("tax_rank")
}

#' @export
tax_rank.default <- function(x, db = NULL, rows = NA, ...) {
  stop("no 'tax_rank' method for ", class(x), call. = FALSE)
}

#' @export
tax_rank.default <- function(x, db = NULL, rows = NA, ...) {
  stats::setNames(tax_rank_(x, ...), x)
}

#' @export
tax_rank.character <- function(x, db = NULL, rows = NA, ...) {
  nstop(db)
  stopifnot(length(db) == 1)
  switch(
    db,
    bold = stats::setNames(tax_rank_(process_ids(x, db, get_boldid,
      rows = rows), ...), x),
    col = stats::setNames(tax_rank_(process_ids(x, db, get_colid,
      rows = rows), ...), x),
    eol = stats::setNames(tax_rank_(process_ids(x, db, get_eolid,
      rows = rows), ...), x),
    gbif = stats::setNames(tax_rank_(process_ids(x, db, get_gbifid,
      rows = rows), ...), x),
    natserv = stats::setNames(tax_rank_(process_ids(x, db, get_natservid,
      rows = rows), ...), x),
    nbn = stats::setNames(tax_rank_(process_ids(x, db, get_nbnid,
      rows = rows), ...), x),
    tol = stats::setNames(tax_rank_(process_ids(x, db, get_tolid,
      rows = rows), ...), x),
    tropicos = stats::setNames(tax_rank_(process_ids(x, db, get_tpsid,
      rows = rows), ...), x),
    itis = stats::setNames(tax_rank_(process_ids(x, db, get_tsn,
      rows = rows), ...), x),
    ncbi = stats::setNames(tax_rank_(process_ids(x, db, get_uid,
      rows = rows), ...), x),
    worms = stats::setNames(tax_rank_(process_ids(x, db, get_wormsid,
      rows = rows), ...), x),
    stop("the provided db value was not recognised", call. = FALSE)
  )
}

#' @export
tax_rank.numeric <- function(x, db = NULL, rows = NA, ...) {
  tax_rank(as.character(x), db, rows, ...)
}

# ---------
tax_rank_ <- function(id, ...) {
  fun <- function(x, clz, ...) {
    res <- classification(x, db = clz, ...)
    if (all(is.na(res))) {
      NA_character_
    } else {
      if (NROW(res[[1]]) > 0) {
        tt <- res[[1]]
        out <- tt[nrow(tt), "rank"][[1]]
        if (length(out) == 0) NA_character_ else tolower(out)
      } else {
        NA_character_
      }
    }
  }
  lapply(id, fun, clz = dbswap(class(id)), ...)
}
