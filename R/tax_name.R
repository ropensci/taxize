#' Get taxonomic names for a given rank
#'
#' @export
#' @param query (character) Vector of taxonomic names to query. required.
#' @param get (character) The ranks of the taxonomic name to get, see
#' \code{\link{rank_ref}}. required.
#' @param db (character) The database to search from: 'itis', 'ncbi' or 'both'.
#'  If 'both' both NCBI and ITIS will be queried. Result will be the union of
#'  both.
#' @param pref (character) If db = 'both', sets the preference for the union.
#' Either 'ncbi' (default) or 'itis'. Currently not implemented.
#' @param verbose (logical) If \code{TRUE} the actual taxon queried is printed
#' on the console.
#' @param ... Other arguments passed to \code{\link{get_tsn}} or
#' \code{\link{get_uid}}.
#'
#' @return A data.frame with one column for every queried rank, in addition to
#' a column for db and queried term.
#'
#' @note While \code{\link{tax_rank}} returns the actual rank of a
#' taxon, \code{\link{tax_name}} searches and returns any specified rank
#' higher in taxonomy.
#'
#' @seealso \code{\link{classification}}
#'
#' @examples \dontrun{
#' # A case where itis and ncbi use the same names
#' tax_name(query = "Helianthus annuus", get = "family", db = "itis")
#' tax_name(query = "Helianthus annuus", get = "family", db = "ncbi")
#' tax_name(query = "Helianthus annuus", get = c("genus","family","order"),
#'   db = "ncbi")
#'
#' # Case where itis and ncbi use different names
#' tax_name(query = "Helianthus annuus", get = "kingdom", db = "itis")
#' tax_name(query = "Helianthus annuus", get = "kingdom", db = "ncbi")
#'
#' # multiple rank arguments
#' tax_name(query = c("Helianthus annuus","Baetis rhodani"), get = c("genus",
#' "kingdom"), db = "ncbi")
#' tax_name(query = c("Helianthus annuus","Baetis rhodani"), get = c("genus",
#' "kingdom"), db = "itis")
#'
#' # query both sources
#' tax_name(query=c("Helianthus annuus", 'Baetis rhodani'), get=c("genus",
#' "kingdom"), db="both")
#' }

tax_name <- function(query, get, db = "itis", pref = 'ncbi', verbose = TRUE,
                     ...) {

  if (missing(get)) stop("you must supply a 'get' value", call. = FALSE)
  db <- match.arg(db, c('itis', 'ncbi', 'both'))
  if (db == 'both' & !pref %in% c('ncbi', 'itis')) {
    stop("if db=both, pref must be either 'itis' or 'ncbi'!\n", call. = FALSE)
  }

  fun <- function(query, get, db, verbose, ...){
    # NCBI
    if (db == "ncbi") return( do_ncbi(query, get, verbose, ...) )
    # ITIS
  	if (db == "itis") return( do_itis(query, get, verbose, ...) )
    # combine both
    if (db == 'both') {
      match_uid <- do_ncbi(query, get, verbose, TRUE, ...)
      match_tsn <- do_itis(query, get, verbose, TRUE, ...)
      setNames(
        data.frame(rbind(t(c("itis", match_tsn)), t(c("ncbi", match_uid))),
                   stringsAsFactors = FALSE), c("db", "query", get))
    }
  }
  plyr::ldply(query, .fun = fun, get = get, db = db, verbose = verbose, ...)
}

do_ncbi <- function(query, get, verbose, both=FALSE, ...) {
  uid <- get_uid(query, verbose = verbose, ...)
  if (is.na(uid)) {
    if (verbose) message("No UID found for species '", query, "'!\n")
    if (both) c(query, rep(NA, length(get))) else setNames(data.frame(t(c("ncbi", query, rep(NA, length(get))))), c("db", "query", get))
  } else {
    hierarchy <- classification(uid, ...)[[1]]
    if (all(is.na(hierarchy))) return(NULL)
    match <- hierarchy$name[match(tolower(get), tolower(hierarchy$rank))]
    if (both) c(query, match) else setNames(data.frame(t(c("ncbi", query, match)), stringsAsFactors = FALSE), c("db", "query", get))
  }
}

do_itis <- function(query, get, verbose, both = FALSE, ...){
  tsn <- get_tsn(query, searchtype = "scientific", verbose = verbose, ...)
  if (is.na(tsn)) {
    if (verbose) message("No TSN found for species '", query, "'!\n")
    if (both) c(query, rep(NA, length(get))) else setNames(data.frame(t(c("itis", query, rep(NA, length(get)))), stringsAsFactors = FALSE), c("db", "query", get))
  } else {
    tt <- classification(tsn, verbose = verbose, ...)[[1]]
    if (all(is.na(tt))) {
      warning(sprintf("%s: no hierarchy data found in ITIS", query), call. = FALSE)
      setNames(data.frame(t(c("itis", query, rep(NA, length(get)))), stringsAsFactors = FALSE), c("db", "query", get))
    } else {
      if (max(unlist(Filter(function(x) length(x) > 0, sapply(get, which_rank)))) <
          which_rank(tt$rank[1])) {
        warning(sprintf("%s: highest rank of ITIS classification is '%s'", query, tt$rank[1]), call. = FALSE)
      }
      if (!all(tolower(get) %in% tolower(tt$rank))) {
        warning(
          sprintf("%s: rank requested ('%s') not in ITIS classification",
                  query,
                  paste0(tolower(get)[!tolower(get) %in% tolower(tt$rank)], collapse = ", ")),
          call. = FALSE)
      }
      if (both) {
        c(query, tt$name[match(tolower(get), tolower(tt$rank))])
      } else {
        out <- tt[tolower(tt$rank) %in% tolower(get), ]$name
        if (length(out) == 0) out <- rep(NA_character_, length(get))
        rname <- tolower(tt[tolower(tt$rank) %in% tolower(get), ]$rank)
        if (length(rname) == 0) rname <- get
        setNames(
          data.frame(t(c("itis", query, out)), stringsAsFactors = FALSE),
          c("db", "query", rname)
        )
      }
    }
  }
}
