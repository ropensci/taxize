#' @title Retrieve the upstream taxa for a given taxon name or ID.
#'
#' @description This function uses a while loop to continually collect taxa up to the
#' taxonomic rank that you specify in the `upto` parameter. You can get data
#' from ITIS (itis) only currently. There is no method exposed by itis
#' for getting taxa at a specific taxonomic rank, so we do it ourselves inside
#' the function.
#'
#' @export
#' @param sci_id Vector of taxa names (character) or IDs (character or numeric) to
#' query.
#' @param db character; database to query. One or both of `itis`. Note
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
#' taxonomic id of any of the acceptable classes: tsn.
#' @param x Deprecated, see `sci_id`
#' @param ... Further args passed on to [itis_downstream()]
#'
#' @return A named list of data.frames with the upstream names of every
#' supplied taxa. You get an NA if there was no match in the database.
#'
#' @examples \dontrun{
#' upstream('Pinus contorta', db = 'itis', upto = 'genus')
#' }
upstream <- function(...) {
  UseMethod("upstream")
}

#' @export
#' @rdname upstream
upstream.default <- function(sci_id, db = NULL, upto = NULL, rows = NA,
  x = NULL, ...) {

  nstop(upto, "upto")
  nstop(db)
  pchk(x, "sci_id")
  if (!is.null(x)) sci_id <- x
  switch(
    db,
    itis = {
      id <- process_stream_ids(sci_id, db, get_tsn, rows = rows, ...)
      setNames(upstream(id, upto = tolower(upto), ...), sci_id)
    },
    stop("the provided db value was not recognised", call. = FALSE)
  )
}

#' @export
#' @rdname upstream
upstream.tsn <- function(sci_id, db = NULL, upto = NULL, ...) {
  warn_db(list(db = db), "itis")
  fun <- function(y, ...){
    # return NA if NA is supplied
    if (is.na(y)) {
      NA
    } else {
      class <- classification(y, ...)
      toget <- class[[1]][ grep(upto, class[[1]]$rank) - 1, c("name", "id") ]
      setNames(downstream(as.tsn(toget$id), downto = upto, ...), toget$name)
    }
  }
  out <- if (length(sci_id) > 1) lapply(sci_id, fun, ...) else fun(sci_id, ...)
  structure(out, class = 'upstream', db = 'itis')
}

#' @export
#' @rdname upstream
upstream.ids <- function(sci_id, db = NULL, upto = NULL, ...) {
  fun <- function(y, ...){
    # return NA if NA is supplied
    if (is.na(y)) {
      out <- NA
    } else {
      out <- upstream(y, upto = tolower(upto), ...)
    }
    return(out)
  }
  out <- if (length(sci_id) > 1) lapply(sci_id, fun, ...) else fun(sci_id, ...)
  structure(out, class = 'downstream_ids')
}
