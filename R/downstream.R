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
#' @param intermediate (logical) If TRUE, return a list of length two with target
#' taxon rank names, with additional list of data.frame's of intermediate
#' taxonomic groups. Default: FALSE
#' @param rows (numeric) Any number from 1 to inifity. If the default NA, all rows are
#' considered. Note that this parameter is ignored if you pass in a taxonomic id of any of the
#' acceptable classes: tsn, colid.
#' @param ... Further args passed on to \code{itis_downstream} or \code{col_downstream}
#'
#' @return A named list of data.frames with the downstream names of every supplied taxa.
#' You get an NA if there was no match in the database.
#'
#' @export
#' @examples \dontrun{
#' # Plug in taxon names
#' downstream("Insecta", db = 'col', downto = 'Order')
#' downstream("Apis", db = 'col', downto = 'Species')
#' downstream("Apis", db = 'itis', downto = 'Species')
#' downstream(c("Apis","Epeoloides"), db = 'itis', downto = 'Species')
#' downstream(c("Apis","Epeoloides"), db = 'col', downto = 'Species')
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
#'
#' # Collect intermediate names
#' ## itis
#' downstream('Bangiophyceae', db="itis", downto="Genus")
#' downstream('Bangiophyceae', db="itis", downto="Genus", intermediate=TRUE)
#' downstream(get_tsn('Bangiophyceae'), downto="Genus")
#' downstream(get_tsn('Bangiophyceae'), downto="Genus", intermediate=TRUE)
#' ## col
#' downstream(get_colid("Animalia"), downto="Class")
#' downstream(get_colid("Animalia"), downto="Class", intermediate=TRUE)
#'
#' # Use the rows parameter
#' ## note how in the second function call you don't get the prompt
#' downstream("Poa", db = 'col', downto="Species")
#' downstream("Poa", db = 'col', downto="Species", rows=1)
#'
#' # use curl options
#' res <- downstream("Apis", db = 'col', downto = 'Species', config=verbose())
#' res <- downstream("Apis", db = 'itis', downto = 'Species', config=verbose())
#' }
downstream <- function(...){
  UseMethod("downstream")
}

#' @export
#' @rdname downstream
downstream.default <- function(x, db = NULL, downto = NULL, intermediate = FALSE, rows=NA, ...){
  nstop(downto, "downto")
  nstop(db)
  switch(db,
         itis = {
           id <- get_tsn(x, rows=rows, ...)
           setNames(downstream(id, downto = downto, intermediate = intermediate, ...), x)
         },
         col = {
           id <- get_colid(x, rows=rows, ...)
           setNames(downstream(id, downto = downto, intermediate = intermediate, ...), x)
         },
         stop("the provided db value was not recognised", call. = FALSE)
  )
}

#' @export
#' @rdname downstream
downstream.tsn <- function(x, db = NULL, downto = NULL, intermediate = FALSE, ...)
{
  fun <- function(y, downto, intermediate, ...){
    # return NA if NA is supplied
    if (is.na(y)) {
      NA
    } else {
		  itis_downstream(tsns = y, downto = downto, intermediate = intermediate, ...)
    }
  }
  out <- lapply(x, fun, downto=downto, intermediate=intermediate, ...)
  structure(out, class='downstream', db='itis', .Names=x)
}

#' @export
#' @rdname downstream
downstream.colid <- function(x, db = NULL, downto = NULL, intermediate = FALSE, ...)
{
  fun <- function(y, downto, intermediate, ...){
    # return NA if NA is supplied
    if(is.na(y)){
      NA
    } else {
      col_downstream(id = y, downto = downto, intermediate = intermediate, ...)
    }
  }
  out <- lapply(x, fun, downto=downto, intermediate=intermediate, ...)
  structure(simp(out), class='downstream', db='col')
}

#' @export
#' @rdname downstream
downstream.ids <- function(x, db = NULL, downto = NULL, intermediate = FALSE, ...)
{
  fun <- function(y, downto, intermediate, ...){
    # return NA if NA is supplied
    if (is.na(y)) {
      NA
    } else {
      downstream(y, downto = downto, intermediate=intermediate, ...)
    }
  }
  structure(lapply(x, fun, downto=downto, intermediate=intermediate, ...), class='downstream_ids')
}

simp <- function(x) if(length(x) == 1) x[[1]] else x
