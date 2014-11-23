#' Retrieve the upstream taxa for a given taxon name or ID.
#'
#' This function uses a while loop to continually collect taxa up to the
#' taxonomic rank that you specify in the \code{upto} parameter. You can get data
#' from ITIS (itis) or Catalogue of Life (col). There is no method exposed by itis
#' or col for getting taxa at a specific taxonomic rank, so we do it ourselves inside
#' the function.
#'
#' @param x character; taxons to query.
#' @param db character; database to query. One or both of \code{itis}, \code{col}.
#' @param upto What taxonomic rank to go down to. One of: 'Superkingdom','Kingdom',
#' 'Subkingdom','Infrakingdom','Phylum','Division','Subphylum','Subdivision','Infradivision',
#' 'Superclass','Class','Subclass','Infraclass','Superorder','Order','Suborder',
#' 'Infraorder','Superfamily','Family','Subfamily','Tribe','Subtribe','Genus','Subgenus',
#' 'Section','Subsection','Species','Subspecies','Variety','Form','Subvariety','Race',
#' 'Stirp','Morph','Aberration','Subform','Unspecified'
#' @param ... Further args passed on to \code{itis_downstream} or \code{col_downstream}
#'
#' @return A named list of data.frames with the upstream names of every supplied taxa.
#' You get an NA if there was no match in the database.
#'
#' @export
#' @examples \donttest{
#' upstream("Pinus contorta", db = 'col', upto = 'Genus') # get all genera at one level up
#' upstream("Abies", db = 'col', upto = 'Genus') # goes to same level, Abies is a genus
#' upstream('Pinus contorta', db = 'col', upto = 'Family')
#' upstream('Poa annua', db = 'col', upto = 'Family')
#' upstream('Poa annua', db = 'col', upto = 'Order')
#'
#' # use itis
#' upstream("Pinus contorta", db = 'itis', upto = 'Genus')
#' }
upstream <- function(...) UseMethod("upstream")

#' @method upstream default
#' @export
#' @rdname upstream
upstream.default <- function(x, db = NULL, upto = NULL, ...){
  if (is.null(upto))
    stop("Must specify upto value!")
  if (is.null(db))
    stop("Must specify db value!")
  if (db == 'itis') {
    id <- get_tsn(x, ...)
    out <- upstream(id, upto = upto, ...)
    names(out) <- x
  }
  if (db == 'col') {
    id <- get_colid(x, ...)
    out <- upstream(id, upto = upto, ...)
    names(out) <- x
  }
  return(out)
}

#' @method upstream tsn
#' @export
#' @rdname upstream
upstream.tsn <- function(x, db = NULL, upto = NULL, ...)
{
  fun <- function(y){
    # return NA if NA is supplied
    if (is.na(y)) { NA } else {
      class <- classification(y, ...)
      toget <- class[[1]][ grep(upto, class[[1]]$rank) - 1, "name" ]
      toget_id <- get_tsn(toget, ...)
      out <- downstream(toget_id, db = "itis", downto = upto, ...)
      names(out) <- toget
      out
    }
  }
  out <- if(length(x) > 1) lapply(x, fun) else fun(x)
  structure(out, class='upstream', db='itis')
#   names(out) <- x
#   class(out) <- 'upstream'
#   attr(out, 'db') <- 'itis'
#   return(out)
}

#' @method upstream colid
#' @export
#' @rdname upstream
upstream.colid <- function(x, db = NULL, upto = NULL, ...) {
  fun <- function(y){
    # return NA if NA is supplied
    if(is.na(y)) { NA } else {
      class <- classification(y)
      toget <- class[[1]][ grep(upto, class[[1]]$rank) - 1, "name" ]
      col_downstream(name = toget, downto = upto, ...)
    }
  }
  out <- if(length(x) > 1) lapply(x, fun) else fun(x)
  structure(out, class='upstream', db='col')
}

#' @method upstream ids
#' @export
#' @rdname upstream
upstream.ids <- function(x, db = NULL, upto = NULL, ...)
{
  fun <- function(y, ...){
    # return NA if NA is supplied
    if (is.na(y)) {
      out <- NA
    } else {
      out <- upstream(y, upto = upto, ...)
    }
    return(out)
  }
  out <- if(length(x) > 1) lapply(x, fun) else fun(x)
  class(out) <- 'downstream_ids'
  return(out)
}
