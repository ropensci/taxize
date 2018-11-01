#' Taxonomic IDs to taxonomic names
#' 
#' @export
#' @param x vector of taxonomic IDs (character or numeric)
#' @param db character; database to query. One or more of \code{tol},
#' or \code{itis}. Note that each taxonomic data source has their own 
#' identifiers, so that if you provide the wrong
#' \code{db} value for the identifier you could get a result, but it will
#' likely be wrong (not what you were expecting).
#' @param ... Further args passed on to \code{tol_id2name} or
#' \code{\link{itis_getrecord}}
#' See those functions for what parameters can be passed on.
#'
#' @return A named list of data.frames
#'
#' @examples \dontrun{
#' id2name(19322, db = "itis")
#' 
#' # with TOL, get NCBI ID and pass to classification()
#' x <- id2name(515698, db = "tol")
#' classification(as.uid(x[[1]]$tax_sources_ncbi))
#' }
id2name <- function(x, db = NULL, ...) UseMethod("id2name")

#' @export
#' @rdname id2name
id2name.default <- function(x, db = NULL, ...) {
  nstop(db)
  if (!db %in% c('tol', 'itis')) {
    stop("'db' must be one of 'tol' or 'itis'")
  }
  id <- process_idn_ids(x, db)
  stats::setNames(id2name(id, ...), x)
  # results <- switch(
  #   db,
  #   tol = {
  #     id <- process_idn_ids(x, db, ...)
  #     stats::setNames(id2name(id, ...), x)
  #   },
  #   itis = {
  #     id <- process_idn_ids(x, db, ...)
  #     stats::setNames(id2name(id, ...), x)
  #   },
  #   stop("the provided db value was not recognised", call. = FALSE)
  # )
  # results
  # set_output_types(results, x, db)
}

# set_output_types <- function(x, x_names, db){
#   blank_fun <- switch(
#     db,
#     itis  = function(x) if (nrow(x) == 0 || is.na(x)) itis_blank else x,
#     col   = function(x) {
#       if (inherits(x, "list")) x <- x[[1]]
#       if (nrow(x) == 0 || is.na(x)) col_blank else x
#     },
#     ncbi  = function(x) if (nrow(x) == 0 || is.na(x)) ncbi_blank else x,
#     worms = function(x) if (nrow(x) == 0 || is.na(x)) worms_blank else x
#   )
# 
#   typed_results <- lapply(seq_along(x), function(i) blank_fun(x[[i]]))
#   names(typed_results) <- x_names
#   attributes(typed_results) <- attributes(x)
#   typed_results
# }

process_idn_ids <- function(input, db) {
  as_fxn <- switch(db, tol = as.tolid, itis = as.tsn)
  as_fxn(input, check = FALSE)
  # g <- tryCatch(as.numeric(as.character(input)), warning = function(e) e)
  # if (is(g, "numeric") || is.character(input) && grepl("[[:digit:]]", input)) {
  #   as_fxn <- switch(db, itis = as.tsn, col = as.colid, worms = as.wormsid)
  #   as_fxn(input, check = FALSE)
  # } else {
  #   eval(fxn)(input, ...)
  # }
}

#' @export
#' @rdname id2name
id2name.tolid <- function(x, ...) {
  fun <- function(y) {
    if (is.na(y)) NA_character_ else tol_id2name(y, ...)
  }
  out <- lapply(x, fun)
  names(out) <- x
  class(out) <- 'id2name'
  attr(out, 'db') <- 'tol'
  return(out)
}

itis_id2name <- function(x, ...) {
  z <- itis_getrecord(x, ...)
  data.frame(tsn = x, name = z$scientificName$combinedName, 
    parent_tsn = z$parentTSN$parentTsn,
    status = z$coreMetadata$taxonUsageRating,
    stringsAsFactors = FALSE)
}

#' @export
#' @rdname id2name
id2name.tsn <- function(x, ...) {
  fun <- function(y) {
    if (is.na(y)) NA_character_ else itis_id2name(y, ...)
  }
  out <- lapply(x, fun)
  names(out) <- x
  class(out) <- 'id2name'
  attr(out, 'db') <- 'tsn'
  return(out)
}
