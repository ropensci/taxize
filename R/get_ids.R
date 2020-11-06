get_ids_dbs <- c(
  "itis", "ncbi", "eol", "tps", "gbif", "nbn", "pow"
)

#' Retrieve taxonomic identifiers for a given taxon name.
#'
#' This is a convenience function to get identifiers across all data sources.
#' You can use other `get_*` functions to get identifiers from specific
#' sources if you like.
#'
#' @export
#' @param sci_com (character) Taxonomic name to query.
#' @param db (character) database to query. One or more of `ncbi`, `itis`, `eol`,
#' `tps`, `gbif`, `nbn`, or `pow`. By default db is set to search
#' all data sources. Note that each taxonomic data source has their own
#' identifiers, so that if you give the wrong `db` value for the identifier you
#' could get a result, it will likely be wrong (not what you were expecting).
#' If using ncbi and/or tps we recommend getting API keys;
#' see [taxize-authentication]
#' @param suppress (logical) suppress \pkg{cli} separators with the database
#' name being queried. default: `FALSE`
#' @param rows (numeric) Any number from 1 to infinity. If the default NA, all
#' rows are returned. When used in `get_ids` this function still only
#' gives back a ids class object with one to many identifiers. See
#' `get_ids_` to get back all, or a subset, of the raw data that you
#' are presented during the ask process.
#' @param names Deprecated, see `sci_com`
#' @param ... Other arguments passed to [get_itis()], [get_ncbi()],
#' [get_eol()], [get_tps()], [get_gbif()],
#' [get_nbn()].
#' @return A vector of taxonomic identifiers, each retaining their respective
#' S3 classes so that each element can be passed on to another function
#' (see e.g.'s).
#' @note There is a timeout of 1/3 seconds between queries to NCBI.
#'
#' @family taxonomic-ids
#' @seealso [classification()]
#'
#' @section Authentication:
#' See [taxize-authentication] for help on authentication
#'
#' @examples \dontrun{
#' # Plug in taxon names directly
#' # specify rows to limit choices available
#' get_ids("Poa annua", db="eol", rows=1)
#' get_ids("Poa annua", db="eol", rows=1:2)
#'
#' ## Or you can specify which source you want via the db parameter
#' get_ids("Chironomus riparius", db = 'ncbi')
#' get_ids("Salvelinus fontinalis", db = 'nbn')
#'
#' get_ids(c("Chironomus riparius", "Pinus contorta"), db = 'ncbi')
#' get_ids("Pinus contorta", db = c('ncbi','eol','tropicos'))
#' get_ids("ava avvva", db = c('ncbi','eol','tropicos'))
#'
#' # Pass on to other functions
#' out <- get_ids("Pinus contorta", db = c('ncbi','eol','tropicos'))
#' classification(out$ncbi)
#'
#' # Get all data back
#' get_ids_(c("Chironomus riparius", "Pinus contorta"), db = 'nbn',
#'   rows=1:10)
#' get_ids_(c("Chironomus riparius", "Pinus contorta"), db = c('itis','gbif'),
#'   rows=1:10)
#'
#' # use curl options
#' get_ids("Agapostemon", db = "ncbi", verbose = TRUE)
#' }

get_ids <- function(sci_com,
  db = c("itis", "ncbi", "eol", "tps", "gbif", "nbn",
    "pow"), suppress = FALSE, names = NULL, ...) {
  
  assert(suppress, "logical")
  if (is.null(db)) stop("Must specify one or more values for db!")
  db <- match.arg(db, choices = get_ids_dbs, several.ok = TRUE)
  foo <- function(x, sci_com, ...){
    if (!suppress) cat_db(x)
    ids <- switch(x,
                  itis = get_itis(sci_com, ...),
                  ncbi = get_ncbi(sci_com, ...),
                  eol = get_eol(sci_com, ...),
                  tps = get_tps(sci_com, ...),
                  gbif = get_gbif(sci_com, ...),
                  nbn = get_nbn(sci_com, ...),
                  pow = get_pow(sci_com, ...))
    names(ids) <- sci_com
    return( ids )
  }

  tmp <- lapply(db, function(x) foo(x, sci_com = sci_com, ...))
  names(tmp) <- db
  class(tmp) <- "ids"
  return( tmp )
}

#' @export
#' @rdname get_ids
get_ids_ <- function(sci_com, db = get_ids_dbs, rows = NA,
  suppress = FALSE, names = NULL, ...) {

  if (is.null(db)) stop("Must specify on or more values for db!")
  db <- match.arg(db, choices = get_ids_dbs, several.ok = TRUE)
  foo <- function(x, sci_com, rows, ...){
    if (!suppress) cat_db(x)
    ids <- switch(x,
                  itis = get_itis_(sci_com, rows = rows, ...),
                  ncbi = get_ncbi_(sci_com, rows = rows, ...),
                  eol = get_eol_(sci_com, rows = rows, ...),
                  tps = get_tps_(sci_com, rows = rows, ...),
                  gbif = get_gbif_(sci_com, rows = rows, ...),
                  nbn = get_nbn_(sci_com, rows = rows, ...))
    stats::setNames(ids, sci_com)
  }
  structure(stats::setNames(
    lapply(db, function(x) foo(x, sci_com = sci_com, rows = rows, ...)), db),
  class = "ids")
}

cat_db <- function(x) {
  cli::cat_line(
    cli::rule(left = paste0(" db: ", x),
      line = 2, line_col = "blue", width = 30)
  )
}
