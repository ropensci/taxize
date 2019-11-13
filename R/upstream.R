#' @title Retrieve the upstream taxa for a given taxon name or ID.
#'
#' @description This function uses a while loop to continually collect taxa up to the
#' taxonomic rank that you specify in the `upto` parameter. You can get data
#' from ITIS (itis) or Catalogue of Life (col). There is no method exposed by itis
#' or col for getting taxa at a specific taxonomic rank, so we do it ourselves inside
#' the function.
#'
#' @export
#' @param x Vector of taxa names (character) or IDs (character or numeric) to
#' query.
#' @param db character; database to query. One or both of `itis`, `col`. Note
#' that each taxonomic data source has their own identifiers, so that if you
#' provide the wrong `db` value for the identifier you could get a result, but
#' it will likely be wrong (not what you were expecting).
#' @param upto What taxonomic rank to go down to. One of: 'superkingdom',
#' 'kingdom', 'subkingdom','infrakingdom','phylum','division','subphylum',
#' 'subdivision','infradivision', 'superclass','class','subclass','infraclass',
#' 'superorder','order','suborder','infraorder','superfamily','family',
#' 'subfamily','tribe','subtribe','genus','subgenus', 'section','subsection',
#' 'species','subspecies','variety','form','subvariety','race', 'stirp',
#' 'morph','aberration','subform', or 'unspecified'
#' @param rows (numeric) Any number from 1 to infinity. If the default NA, all
#' rows are considered. Note that this parameter is ignored if you pass in a
#' taxonomic id of any of the acceptable classes: tsn, colid.
#' @param ... Further args passed on to [itis_downstream()] or
#' [col_downstream()]
#'
#' @return A named list of data.frames with the upstream names of every
#' supplied taxa. You get an NA if there was no match in the database.
#'
#' @examples \dontrun{
#' ## col
#' ### get all genera at one level up
#' upstream("Pinus contorta", db = 'col', upto = 'genus')
#' ### goes to same level, Abies is a genus
#' upstream("Abies", db = 'col', upto = 'genus')
#' upstream('Pinus contorta', db = 'col', upto = 'family')
#' upstream('Poa annua', db = 'col', upto = 'family')
#' upstream('Poa annua', db = 'col', upto = 'order')
#'
#' ## itis
#' upstream(x='Pinus contorta', db = 'itis', upto = 'genus')
#'
#' ## both
#' upstream(get_ids('Pinus contorta', db = c('col','itis')), upto = 'genus')
#'
#' # Use rows parameter to select certain
#' upstream('Poa annua', db = 'col', upto = 'genus')
#' upstream('Poa annua', db = 'col', upto = 'genus', rows=1)
#'
#' # use curl options
#' res <- upstream('Poa annua', db = 'col', upto = 'genus', verbose = TRUE)
#' }
upstream <- function(...) {
  UseMethod("upstream")
}

#' @export
#' @rdname upstream
upstream.default <- function(x, db = NULL, upto = NULL, rows = NA, ...){
  nstop(upto, "upto")
  nstop(db)
  switch(
    db,
    itis = {
      id <- process_stream_ids(x, db, get_tsn, rows = rows, ...)
      setNames(upstream(id, upto = tolower(upto), ...), x)
    },
    col = {
      id <- process_stream_ids(x, db, get_colid, rows = rows, ...)
      setNames(upstream(id, upto = tolower(upto), ...), x)
    },
    stop("the provided db value was not recognised", call. = FALSE)
  )
}

#' @export
#' @rdname upstream
upstream.tsn <- function(x, db = NULL, upto = NULL, ...) {
  warn_db(list(db = db), "itis")
  fun <- function(y, ...){
    # return NA if NA is supplied
    if (is.na(y)) {
      NA
    } else {
      class <- classification(y, ...)
      toget <- class[[1]][ grep(upto, class[[1]]$rank) - 1, c("name", "id") ]
      setNames(downstream(x = as.tsn(toget$id), downto = upto, ...), toget$name)
    }
  }
  out <- if (length(x) > 1) lapply(x, fun, ...) else fun(x, ...)
  structure(out, class = 'upstream', db = 'itis')
}

#' @export
#' @rdname upstream
upstream.colid <- function(x, db = NULL, upto = NULL, ...) {
  warn_db(list(db = db), "col")
  fun <- function(y, ...){
    # return NA if NA is supplied
    if (is.na(y)) { NA } else {
      class <- classification(y, ...)
      toget <- class[[1]][ grep(upto, class[[1]]$rank) - 1, "name" ]
      col_downstream(name = toget, downto = upto, ...)
    }
  }
  out <- if (length(x) > 1) lapply(x, fun, ...) else fun(x, ...)
  structure(out, class = 'upstream', db = 'col')
}

#' @export
#' @rdname upstream
upstream.ids <- function(x, db = NULL, upto = NULL, ...) {
  fun <- function(y, ...){
    # return NA if NA is supplied
    if (is.na(y)) {
      out <- NA
    } else {
      out <- upstream(y, upto = tolower(upto), ...)
    }
    return(out)
  }
  structure(if (length(x) > 1) lapply(x, fun, ...) else fun(x, ...), class = 'downstream_ids')
}
