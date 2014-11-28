#' Retrieve synonyms from various sources given input taxonomic names or identifiers.
#'
#' @param x character; taxons to query.
#' @param db character; database to query. either \code{itis}, \code{tropicos},
#' \code{ubio}, or \code{nbn}.
#' @param id character; identifiers, returned by \code{\link[taxize]{get_tsn}},
#'    \code{\link[taxize]{get_tpsid}}, \code{\link[taxize]{get_ubioid}}, or
#'    \code{\link[taxize]{get_nbnid}}
#' @param ... Other passed arguments.
#'
#' @return A named list of data.frames with the synonyms of every supplied taxa.
#' @note If IDs are supplied directly (not from the \code{get_*} functions) you
#'    must specify the type of ID.
#'
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{get_tpsid}},
#' \code{\link[taxize]{get_ubioid}}, \code{\link[taxize]{get_nbnid}}
#'
#' @export
#' @examples \donttest{
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
#' }

synonyms <- function(...){
  UseMethod("synonyms")
}

#' @export
#' @rdname synonyms
synonyms.default <- function(x, db = NULL, ...){
  if (is.null(db)) stop("Must specify db!", call. = FALSE)
  switch(db,
         itis = {
           id <- get_tsn(x, ...)
           setNames(synonyms(id, ...), x)
         },
         tropicos = {
           id <- get_tpsid(x, ...)
           setNames(synonyms(id, ...), x)
         },
         ubio = {
           id <- get_ubioid(x, searchtype = 'scientific', ...)
           setNames(synonyms(id, ...), x)
         },
         nbn = {
           id <- get_nbnid(x, ...)
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
