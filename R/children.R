#' Retrieve immediate children taxa for a given taxon name or ID.
#'
#' This function is different from \code{\link{downstream}} in that it only collects immediate
#' taxonomic children, while \code{\link{downstream}} collects taxonomic names down to a specified
#' taxonomic rank, e.g., getting all species in a family.
#'
#' @export
#' @param x Vector of taxa names (character) or IDs (character or numeric) to query.
#' @param db character; database to query. One or more of \code{itis}, \code{col}, or \code{ncbi}.
#' Note that each taxonomic data source has their own identifiers, so that if you
#' provide the wrong \code{db} value for the identifier you could get a result,
#' but it will likely be wrong (not what you were expecting).
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
#' # Plug in taxonomic IDs
#' children(161994, db = "itis")
#' children(8028, db = "ncbi")
#' children("578cbfd2674a9b589f19af71a33b89b6", db = "col")
#' ## works with numeric if as character as well
#' children("161994", db = "itis")
#'
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
#' (id <- get_colid("Apis"))
#' children(id)
#' children(get_colid("Apis"))
#'
#' # Many taxa
#' sp <- c("Tragia", "Schistocarpha", "Encalypta")
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
#' library("httr")
#' res <- children("Poa", db = 'col', rows=1, config=verbose())
#' res <- children("Salmo", db = 'itis', config=verbose())
#' res <- children("Salmo", db = 'ncbi', config=verbose())
#' }

children <- function(...){
  UseMethod("children")
}

#' @export
#' @rdname children
children.default <- function(x, db = NULL, rows = NA, ...) {
  nstop(db)
  switch(db,
         itis = {
           id <- process_children_ids(x, db, get_tsn, rows = rows, ...)
           setNames(children(id, ...), x)
         },

         col = {
           id <- process_children_ids(x, db, get_colid, rows = rows, ...)
           setNames(children(id, ...), x)
         },

         ncbi = {
           if (all(grepl("^[[:digit:]]*$", x))) {
             id <- x
             class(id) <- "uid"
             setNames(children(id, ...), x)
           } else {
             out <- ncbi_children(name = x, ...)
             structure(out, class = 'children', db = 'ncbi', .Names = x)
           }
         },
         stop("the provided db value was not recognised", call. = FALSE)
  )
}

process_children_ids <- function(input, db, fxn, ...){
  g <- tryCatch(as.numeric(as.character(input)), warning = function(e) e)
  if (is(g, "numeric") || is.character(input) && grepl("[[:digit:]]", input)) {
    as_fxn <- switch(db, itis = as.tsn, col = as.colid)
    as_fxn(input, check = FALSE)
  } else {
    eval(fxn)(input, ...)
  }
}

#' @export
#' @rdname children
children.tsn <- function(x,  db = NULL, ...) {
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

#' @export
#' @rdname children
children.colid <- function(x,  db = NULL, ...) {
  fun <- function(y){
    # return NA if NA is supplied
    if (is.na(y)) {
      out <- NA
    } else {
      out <- col_children(id = y, ...)
    }
    return(out)
  }
  out <- lapply(x, fun)
  if (length(out) == 1) {
    out = out[[1]]
  }
  class(out) <- 'children'
  attr(out, 'db') <- 'col'
  return(out)
}

#' @export
#' @rdname children
children.ids <- function(x, db = NULL, ...) {
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

#' @export
#' @rdname children
children.uid <- function(x, db = NULL, ...) {
  out <- ncbi_children(id = x, ...)
  class(out) <- 'children'
  attr(out, 'db') <- 'ncbi'
  return(out)
}
