#' Retrieve synonyms from various sources given input taxonomic names or identifiers.
#'
#' @param x character; taxons to query.
#' @param db character; database to query. either \code{itis}, \code{tropicos},
#' \code{ubio}, or \code{nbn}.
#' @param id character; identifiers, returned by \code{\link[taxize]{get_tsn}},
#'    \code{\link[taxize]{get_tpsid}}, \code{\link[taxize]{get_ubioid}}, or
#'    \code{\link[taxize]{get_nbnid}}
#' @param rows (numeric) Any number from 1 to inifity. If the default NA, all rows are
#' considered. Note that this parameter is ignored if you pass in a taxonomic id of any of the
#' acceptable classes: tsn, tpsid, ubioid, nbnid, ids.
#' @param ... Other passed arguments to internal functions \code{get_*()} and functions to
#' gather synonyms.
#'
#' @return A named list of data.frames with the synonyms of every supplied taxa.
#' @note If IDs are supplied directly (not from the \code{get_*} functions) you
#'    must specify the type of ID.
#'
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{get_tpsid}},
#' \code{\link[taxize]{get_ubioid}}, \code{\link[taxize]{get_nbnid}}
#'
#' @export
#' @examples \dontrun{
#' # Plug in taxon names directly
#' synonyms("Poa annua", db="itis")
#' synonyms(c("Poa annua",'Pinus contorta','Puma concolor'), db="itis")
#' synonyms("Poa annua", db="tropicos")
#' synonyms("Pinus contorta", db="tropicos")
#' synonyms(c("Poa annua",'Pinus contorta'), db="tropicos")
#' synonyms("Salmo friderici", db='ubio')
#' synonyms(c("Salmo friderici",'Carcharodon carcharias','Puma concolor'), db="ubio")
#' synonyms("Pinus sylvestris", db='nbn')
#'
#' # Use get_* methods
#' synonyms(get_tsn("Poa annua"))
#' synonyms(get_tpsid("Poa annua"))
#' synonyms(get_ubioid("Carcharodon carcharias"))
#' synonyms(get_nbnid("Carcharodon carcharias"))
#'
#' # Pass many ids from class "ids"
#' out <- get_ids(names="Poa annua", db = c('itis','tropicos'))
#' synonyms(out)
#'
#' # Use the rows parameter to select certain rows
#' synonyms("Poa annua", db='tropicos', rows=1)
#' synonyms("Poa annua", db='tropicos', rows=1:3)
#' synonyms("Pinus sylvestris", db='nbn', rows=1:3)
#' }

synonyms <- function(...){
  UseMethod("synonyms")
}

#' @export
#' @rdname synonyms
synonyms.default <- function(x, db = NULL, rows = NA, ...){
  nstop(db)
  switch(db,
         itis = {
           id <- get_tsn(x, rows = rows, ...)
           setNames(synonyms(id, ...), x)
         },
         tropicos = {
           id <- get_tpsid(x, rows = rows, ...)
           setNames(synonyms(id, ...), x)
         },
         ubio = {
           id <- get_ubioid(x, searchtype = 'scientific', rows = rows, ...)
           setNames(synonyms(id, ...), x)
         },
         nbn = {
           id <- get_nbnid(x, rows = rows, ...)
           setNames(synonyms(id, ...), x)
         },
         stop("the provided db value was not recognised", call. = FALSE)
  )
}

#' @export
#' @rdname synonyms
synonyms.tsn <- function(id, ...)
{
  fun <- function(x){
    if (is.na(x)) { NA } else {
      out <- getsynonymnamesfromtsn(x, ...)
      if(as.character(out[1,1]) == 'nomatch') names(out) <- c('name','tsn')
      out
    }
  }
  tmp <- lapply(id, fun)
  names(tmp) <- id
  return(tmp)
}

#' @export
#' @rdname synonyms
synonyms.tpsid <- function(id, ...)
{
  fun <- function(x){
    if (is.na(x)) { NA } else {
      tp_synonyms(x, ...)$synonyms
    }
  }
  tmp <- lapply(id, fun)
  names(tmp) <- id
  return(tmp)
}

#' @export
#' @rdname synonyms
synonyms.ubioid <- function(id, ...)
{
  fun <- function(x){
    if (is.na(x)) { NA  } else {
      ubio_id(namebankID = x, ...)[['synonyms']]
    }
  }
  tmp <- lapply(id, fun)
  names(tmp) <- id
  return(tmp)
}

#' @export
#' @rdname synonyms
synonyms.nbnid <- function(id, ...)
{
  fun <- function(x){
    if (is.na(x)) { NA } else {
      nbn_synonyms(x, ...)
    }
  }
  tmp <- lapply(id, fun)
  names(tmp) <- id
  return(tmp)
}

#' @export
#' @rdname synonyms
synonyms.ids <- function(id, ...)
{
  fun <- function(x){
    if (is.na(x)) {
      out <- NA
    } else {
      out <- synonyms(x, ...)
    }
    return( out )
  }
  return( lapply(id, fun) )
}
