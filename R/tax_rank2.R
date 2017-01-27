#' get rank names
#'
#' @export
#' @param x (character) Vector of one or more taxon names (character) or
#' IDs (character or numeric) to query.
#' @param db (character) database to query. either \code{ncbi}, \code{itis},
#' \code{eol}, \code{col}, \code{tropicos}, \code{gbif}, \code{nbn},
#' \code{worms}, \code{natserv}, \code{bold}. Note that each taxonomic data
#' source has their own identifiers, so that if you provide the wrong
#' \code{db} value for the identifier you could get a result, but it will
#' likely be wrong (not what you were expecting).
#' @param ... Additional arguments to \code{\link{classification}}
#' @return A named list of character vectors with ranks (all lower-cased)
#' @examples \dontrun{
#' tax_rank2(x = "Helianthus annuus", db = "itis")
#' tax_rank2(get_tsn("Helianthus annuus"))
#' tax_rank2(c("Helianthus", "Pinus", "Poa"), db = "itis")
#'
#' tax_rank2(get_boldid("Helianthus annuus"))
#' tax_rank2("421377", db = "bold")
#' tax_rank2(421377, db = "bold")
#'
#' tax_rank2(c("Helianthus annuus", "Puma", "Homo sapiens"), db = 'ncbi')
#' tax_rank2(c("Helianthus annuus", "Quercus", "Fabaceae"), db = 'tropicos')
#'
#' tax_rank2(names_list("species"), db = 'gbif')
#' tax_rank2(names_list("family"), db = 'gbif')
#'
#' tax_rank2(c("Platanista gangetica", "Lichenopora neapolitana"), db = "worms")
#' }
tax_rank2 <- function(...) {
  UseMethod("tax_rank2")
}

#' @export
tax_rank2.default <- function(x, db = NULL, ...) {
  stop("no 'tax_rank2' method for ", class(x), call. = FALSE)
}

#' @export
tax_rank2.default <- function(x, db = NULL, ...) {
  stats::setNames(tax_rank2_(x, ...), x)
}

#' @export
tax_rank2.character <- function(x, db = NULL, ...) {
  nstop(db)
  stopifnot(length(db) == 1)
  switch(
    db,
    bold = stats::setNames(tax_rank2_(process_ids(x, db, get_boldid), ...), x),
    col = stats::setNames(tax_rank2_(process_ids(x, db, get_colid), ...), x),
    eol = stats::setNames(tax_rank2_(process_ids(x, db, get_eolid), ...), x),
    gbif = stats::setNames(tax_rank2_(process_ids(x, db, get_gbifid), ...), x),
    natserv = stats::setNames(tax_rank2_(process_ids(x, db, get_natservid), ...), x),
    nbn = stats::setNames(tax_rank2_(process_ids(x, db, get_nbnid), ...), x),
    tol = stats::setNames(tax_rank2_(process_ids(x, db, get_tolid), ...), x),
    tropicos = stats::setNames(tax_rank2_(process_ids(x, db, get_tpsid), ...), x),
    itis = stats::setNames(tax_rank2_(process_ids(x, db, get_tsn), ...), x),
    ncbi = stats::setNames(tax_rank2_(process_ids(x, db, get_uid), ...), x),
    worms = stats::setNames(tax_rank2_(process_ids(x, db, get_wormsid), ...), x),
    stop("the provided db value was not recognised", call. = FALSE)
  )
}

#' @export
tax_rank2.numeric <- function(x, db = NULL, ...) {
  tax_rank2(as.character(x), db, ...)
}

# ---------
tax_rank2_ <- function(id, ...) {
  fun <- function(x, clz, ...) {
    res <- classification(x, db = clz, ...)
    if (is.na(res)) {
      NA_character_
    } else {
      if (NROW(res[[1]]) > 0) {
        tt <- res[[1]]
        out <- tt[nrow(tt), 'rank'][[1]]
        if (length(out) == 0) NA_character_ else tolower(out)
      } else {
        NA_character_
      }
    }
  }
  lapply(id, fun, clz = dbswap(class(id)), ...)
}
