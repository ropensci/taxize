#' Retrieve immediate children taxa for a given taxon name or ID.
#'
#' This function is different from \code{\link{downstream}} in that it only collects immediate
#' taxonomic children, while \code{\link{downstream}} collects taxonomic names down to a specified
#' taxonomic rank, e.g., getting all species in a family.
#'
#' @export
#'
#' @param x character; taxons to query.
#' @param db character; database to query. One or more of \code{itis}, \code{col}, or \code{ncbi}.
#' @param rows (numeric) Any number from 1 to inifity. If the default NA, all rows are
#' considered. Note that this parameter is ignored if you pass in a taxonomic id of any of the
#' acceptable classes: tsn, colid. NCBI has a method for this function but rows doesn't work.
#' @param ... Further args passed on to \code{\link{col_children}},
#' \code{\link{gethierarchydownfromtsn}}, or \code{\link{ncbi_children}}.
#' See those functions for what parameters can be passed on.
#'
#' @return A named list of data.frames with the children names of every supplied taxa.
#' You get an NA if there was no match in the database.
#'
#' @examples \dontrun{
#' # Plug in taxon names
#' children("Salmo", db = 'col')
#' children("Salmo", db = 'itis')
#' children("Salmo", db = 'ncbi')
#'
#' # Plug in IDs
#' (id <- get_colid("Apis"))
#' children(id)
#'
#' ## Equivalently, plug in the call to get the id via e.g., get_colid into children
#' identical(children(id), children(get_colid("Apis")))
#'
#' (id <- get_colid("Apis"))
#' children(id)
#' children(get_colid("Apis"))
#'
#' # Many taxa
#' (sp <- names_list("genus", 3))
#' children(sp, db = 'col')
#' children(sp, db = 'itis')
#'
#' # Two data sources
#' (ids <- get_ids("Apis", db = c('col','itis')))
#' children(ids)
#' ## same result
#' children(get_ids("Apis", db = c('col','itis')))
#'
#' # Use the rows parameter
#' children("Poa", db = 'col')
#' children("Poa", db = 'col', rows=1)
#'
#' # use curl options
#' res <- children("Poa", db = 'col', rows=1, config=verbose())
#' res <- children("Salmo", db = 'itis', config=verbose())
#' res <- children("Salmo", db = 'ncbi', config=verbose())
#' }

children <- function(...){
  UseMethod("children")
}

#' @method children default
#' @export
#' @rdname children
children.default <- function(x, db = NULL, rows = NA, ...)
{
  nstop(db)
  switch(db,
         itis = {
           id <- get_tsn(x, rows = rows, ...)
           setNames(children(id, ...), x)
         },

         col = {
           id <- get_colid(x, rows = rows, ...)
           setNames(children(id, ...), x)
         },

         ncbi = {
           if (all(grepl("^[[:digit:]]*$", x))) {
             id <- x
             class(id) <- "uid"
             setNames(children(id, ...), x)
           } else {
             out <- ncbi_children(name = x, ...)
             structure(out, class='children', db='ncbi', .Names=x)
           }
         },

#        ubio = {
#          id <- get_ubioid(x, ...)
#          out <- children(id, ...)
#          names(out) <- x
#        },

         stop("the provided db value was not recognised", call. = FALSE)
  )
}

#' @method children tsn
#' @export
#' @rdname children
children.tsn <- function(x,  db = NULL, ...)
{
  fun <- function(y){
    # return NA if NA is supplied
    if (is.na(y)) {
      out <- NA
    } else {
		  out <- gethierarchydownfromtsn(tsn = y, ...)
    }
  }
  out <- lapply(x, fun)
  names(out) <- x
  class(out) <- 'children'
  attr(out, 'db') <- 'itis'
  return(out)
}

#' @method children colid
#' @export
#' @rdname children
children.colid <- function(x,  db = NULL, ...) {
  fun <- function(y){
    # return NA if NA is supplied
    if(is.na(y)){
      out <- NA
    } else {
      out <- col_children(id = y, ...)
    }
    return(out)
  }
  out <- lapply(x, fun)
  if(length(out)==1){ out=out[[1]] } else { out=out }
  class(out) <- 'children'
  attr(out, 'db') <- 'col'
  return(out)
}

# children.ubioid <- function(x,  db = NULL, ...) {
#   fun <- function(y){
#     # return NA if NA is supplied
#     if(is.na(y)){
#       out <- NA
#     } else {
#       hierid <- ubio_classification_search(namebankID = y)
#       hierid <- hierid[ grep(104, hierid$classificationtitleid), 'hierarchiesid' ]
#       out <- ubio_classification(hierarchiesID = hierid, childrenFlag = 1, ...)[['children']]
#     }
#     return(out)
#   }
#   out <- lapply(x, fun)
#   class(out) <- 'children'
#   attr(out, 'db') <- 'ubio'
#   return(out)
# }

#' @method children ids
#' @export
#' @rdname children
children.ids <- function(x, db = NULL, ...)
{
  fun <- function(y, ...){
    # return NA if NA is supplied
    if (is.na(y)) {
      out <- NA
    } else {
      out <- children(y, ...)
    }
    return(out)
  }
  out <- lapply(x, fun)
  class(out) <- 'children_ids'
  return(out)
}

#' @method children uid
#' @export
#' @rdname children
children.uid <- function(x, db = NULL, ...)
{
  out <- ncbi_children(id = x, ...)
  class(out) <- 'children'
  attr(out, 'db') <- 'ncbi'
  return(out)
}
