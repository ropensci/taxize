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
#' @param rows (numeric) Any number from 1 to inifity. If the default NA, all rows are
#' considered. Note that this parameter is ignored if you pass in a taxonomic id of any of the
#' acceptable classes: tsn, colid.
#' @param ... Further args passed on to \code{itis_downstream} or \code{col_downstream}
#'
#' @return A named list of data.frames with the upstream names of every supplied taxa.
#' You get an NA if there was no match in the database.
#'
#' @export
#' @examples \dontrun{
#' ## col
#' upstream("Pinus contorta", db = 'col', upto = 'Genus') # get all genera at one level up
#' upstream("Abies", db = 'col', upto = 'Genus') # goes to same level, Abies is a genus
#' upstream('Pinus contorta', db = 'col', upto = 'Family')
#' upstream('Poa annua', db = 'col', upto = 'Family')
#' upstream('Poa annua', db = 'col', upto = 'Order')
#'
#' ## itis
#' upstream(x='Pinus contorta', db = 'itis', upto = 'Genus')
#'
#' ## both
#' upstream(get_ids('Pinus contorta', db = c('col','itis')), upto = 'Genus')
#'
#' # Use rows parameter to select certain
#' upstream('Poa annua', db = 'col', upto = 'Genus')
#' upstream('Poa annua', db = 'col', upto = 'Genus', rows=1)
#'
#' # use curl options
#' res <- upstream('Poa annua', db = 'col', upto = 'Genus', config=verbose())
#' }
upstream <- function(...) UseMethod("upstream")

#' @export
#' @rdname upstream
upstream.default <- function(x, db = NULL, upto = NULL, rows = NA, ...){
  nstop(upto, "upto")
  nstop(db)
  switch(db,
         itis = {
           id <- get_tsn(x, rows = rows, ...)
           setNames(upstream(id, upto = upto, ...), x)
         },
         col = {
           id <- get_colid(x, rows = rows, ...)
           setNames(upstream(id, upto = upto, ...), x)
         },
         stop("the provided db value was not recognised", call. = FALSE)
  )
}

#' @export
#' @rdname upstream
upstream.tsn <- function(x, db = NULL, upto = NULL, ...)
{
  fun <- function(y, ...){
    # return NA if NA is supplied
    if (is.na(y)) { NA } else {
      class <- classification(y, ...)
      toget <- class[[1]][ grep(upto, class[[1]]$rank) - 1, c("name","id") ]
      setNames(downstream(x=as.tsn(toget$id), downto = upto, ...), toget$name)
    }
  }
  out <- if(length(x) > 1) lapply(x, fun, ...) else fun(x, ...)
  structure(out, class='upstream', db='itis')
}

#' @export
#' @rdname upstream
upstream.colid <- function(x, db = NULL, upto = NULL, ...) {
  fun <- function(y, ...){
    # return NA if NA is supplied
    if(is.na(y)) { NA } else {
      class <- classification(y, ...)
      toget <- class[[1]][ grep(upto, class[[1]]$rank) - 1, "name" ]
      col_downstream(name = toget, downto = upto, ...)
    }
  }
  out <- if(length(x) > 1) lapply(x, fun, ...) else fun(x, ...)
  structure(out, class='upstream', db='col')
}

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
  structure(if(length(x) > 1) lapply(x, fun, ...) else fun(x, ...), class='downstream_ids')
}
