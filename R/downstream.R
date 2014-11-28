#' Retrieve the downstream taxa for a given taxon name or ID.
#'
#' This function uses a while loop to continually collect children taxa down to the
#' taxonomic rank that you specify in the \code{downto} parameter. You can get data
#' from ITIS (itis) or Catalogue of Life (col). There is no method exposed by itis
#' or col for getting taxa at a specific taxonomic rank, so we do it ourselves inside
#' the function.
#'
#' @param x character; taxons to query.
#' @param db character; database to query. One or both of \code{itis}, \code{col}.
#' @param downto What taxonomic rank to go down to. One of: 'Superkingdom','Kingdom',
#' 'Subkingdom','Infrakingdom','Phylum','Division','Subphylum','Subdivision','Infradivision',
#' 'Superclass','Class','Subclass','Infraclass','Superorder','Order','Suborder',
#' 'Infraorder','Superfamily','Family','Subfamily','Tribe','Subtribe','Genus','Subgenus',
#' 'Section','Subsection','Species','Subspecies','Variety','Form','Subvariety','Race',
#' 'Stirp','Morph','Aberration','Subform','Unspecified'
#' @param ... Further args passed on to \code{itis_downstream} or \code{col_downstream}
#'
#' @return A named list of data.frames with the downstream names of every supplied taxa.
#' You get an NA if there was no match in the database.
#'
#' @export
#' @examples \donttest{
#' # Plug in taxon names
#' downstream("Insecta", db = 'col', downto = 'Order')
#' downstream("Apis", db = 'col', downto = 'Species')
#'
#' # Plug in IDs
#' id <- get_colid("Apis")
#' downstream(id, downto = 'Species')
#'
#' ## Equivalently, plug in the call to get the id via e.g., get_colid into downstream
#' identical(downstream(id, downto = 'Species'),
#'          downstream(get_colid("Apis"), downto = 'Species'))
#'
#' id <- get_colid("Apis")
#' downstream(id, downto = 'Species')
#' downstream(get_colid("Apis"), downto = 'Species')
#'
#' # Many taxa
#' sp <- names_list("genus", 3)
#' downstream(sp, db = 'col', downto = 'Species')
#' downstream(sp, db = 'itis', downto = 'Species')
#'
#' # Both data sources
#' ids <- get_ids("Apis", db = c('col','itis'))
#' downstream(ids, downto = 'Species')
#' ## same result
#' downstream(get_ids("Apis", db = c('col','itis')), downto = 'Species')
#' }
downstream <- function(...){
  UseMethod("downstream")
}

#' @export
#' @rdname downstream
downstream.default <- function(x, db = NULL, downto = NULL, ...){
  nstop(downto, "downto")
  nstop(db)
  switch(db,
         itis = {
           id <- get_tsn(x, ...)
           setNames(downstream(id, downto = downto, ...), x)
         },
         col = {
           id <- get_colid(x, ...)
           setNames(downstream(id, downto = downto, ...), x)
         },
         stop("the provided db value was not recognised", call. = FALSE)
  )
}

#' @export
#' @rdname downstream
downstream.tsn <- function(x,  db = NULL, ...)
{
  fun <- function(y){
    # return NA if NA is supplied
    if (is.na(y)) {
      out <- NA
    } else {
		  out <- itis_downstream(tsns = y, ...)
    }
  }
  out <- lapply(x, fun)
  names(out) <- x
  class(out) <- 'downstream'
  attr(out, 'db') <- 'itis'
  return(out)
}

#' @export
#' @rdname downstream
downstream.colid <- function(x,  db = NULL, ...) {
  fun <- function(y){
    # return NA if NA is supplied
    if(is.na(y)){
      out <- NA
    } else {
      out <- col_downstream(id = y, ...)
    }
    return(out)
  }
  out <- lapply(x, fun)
  if(length(out)==1){ out=out[[1]] } else { out=out }
  class(out) <- 'downstream'
  attr(out, 'db') <- 'col'
  return(out)
}

#' @export
#' @rdname downstream
downstream.ids <- function(x, db = NULL, downto, ...)
{
  fun <- function(y, ...){
    # return NA if NA is supplied
    if (is.na(y)) {
      out <- NA
    } else {
      out <- downstream(y, downto = downto, ...)
    }
    return(out)
  }
  out <- lapply(x, fun)
  class(out) <- 'downstream_ids'
  return(out)
}
