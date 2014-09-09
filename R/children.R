#' Retrieve immediate children taxa for a given taxon name or ID.
#'
#' This function is different from \code{downstream()} in that it only collects immediate
#' taxonomic children, while \code{downstream()} collects taxonomic names down to a specified
#' taxonomic rank, e.g., getting all species in a family.
#'
#' @export
#' 
#' @param x character; taxons to query.
#' @param db character; database to query. One or more of \code{itis}, or \code{col}.
#' @param ... Further args passed on to \code{col_children}, or
#' \code{gethierarchydownfromtsn}. See those functions for what parameters can be passed on.
#'
#' @return A named list of data.frames with the children names of every supplied taxa.
#' You get an NA if there was no match in the database.
#'
#' @examples \donttest{
#' # Plug in taxon names
#' children("Salmo", db = 'col')
#' children("Salmo", db = 'itis')
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
#' }

children <- function(...){
  UseMethod("children")
}

#' @method children default
#' @export
#' @rdname children
children.default <- function(x, db = NULL, ...)
{
  if (is.null(db))
    stop("Must specify db value!")
  
  if (db == 'itis') {
    id <- get_tsn(x, ...)
    out <- children(id, ...)
    names(out) <- x
  }
  if (db == 'col') {
    id <- get_colid(x, ...)
    out <- children(id, ...)
    names(out) <- x
  }
#   if (db == 'ubio') {
#     id <- get_ubioid(x, ...)
#     out <- children(id, ...)
#     names(out) <- x
#   }
  return(out)
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
