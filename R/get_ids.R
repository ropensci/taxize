#' Retrieve taxonomic identifiers for a given taxon name.
#'
#' This is a convenience function to get identifiers across all data sources. You can
#' use other \code{get_*} functions to get identifiers from specific sources if
#' you like.
#'
#' @export
#' @param names character; Taxonomic name to query.
#' @param db character; database to query. One or  more of \code{ncbi}, \code{itis},
#'    \code{eol}, \code{col}, \code{tropicos}, \code{gbif}, \code{ubio}, or \code{nbn}. By
#'    default db is set to search all data sources.
#' @param ... Other arguments passed to \code{\link[taxize]{get_tsn}},
#'    \code{\link[taxize]{get_uid}}, \code{\link[taxize]{get_eolid}},
#'    \code{\link[taxize]{get_colid}}, \code{\link[taxize]{get_tpsid}},
#'    \code{\link[taxize]{get_gbifid}}, \code{\link[taxize]{get_ubioid}},
#'    \code{\link[taxize]{get_nbnid}}.
#' @return A vector of taxonomic identifiers, each retaining their respective S3
#'    classes so that each element can be passed on to another function (see e.g.'s).
#' @note There is a timeout of 1/3 seconds between queries to NCBI.
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{get_uid}},
#'    \code{\link[taxize]{get_eolid}}, \code{\link[taxize]{get_colid}},
#'    \code{\link[taxize]{get_tpsid}}, \code{\link[taxize]{get_gbifid}},
#'    \code{\link[taxize]{get_ubioid}}, or \code{\link[taxize]{get_nbnid}}.
#' @examples \donttest{
#' # Plug in taxon names directly
#' ## By default you get ids for all data sources
#' get_ids(names="Chironomus riparius")
#'
#' # specify rows to limit choices available
#' get_ids(names="Poa annua", db=c("col","eol"), rows=1)
#' get_ids(names="Poa annua", db=c("col","eol"), rows=1:2)
#'
#' ## Or you can specify which source you want via the db parameter
#' get_ids(names="Chironomus riparius", db = 'ncbi')
#' get_ids(names="Salvelinus fontinalis", db = 'ubio')
#'
#' get_ids(names="Salvelinus fontinalis", db = 'nbn')
#'
#' get_ids(names=c("Chironomus riparius", "Pinus contorta"), db = 'ncbi')
#' get_ids(names=c("Chironomus riparius", "Pinus contorta"), db = c('ncbi','itis'))
#' get_ids(names=c("Chironomus riparius", "Pinus contorta"), db = c('ncbi','itis','col'))
#' get_ids(names="Pinus contorta", db = c('ncbi','itis','col','eol','tropicos'))
#' get_ids(names="ava avvva", db = c('ncbi','itis','col','eol','tropicos'))
#' get_ids(names="ava avvva", db = c('ncbi','itis','col','eol','tropicos'), verbose=FALSE)
#'
#' # Pass on to other functions
#' out <- get_ids(names="Pinus contorta", db = c('ncbi','itis','col','eol','tropicos'))
#' classification(out$itis)
#' synonyms(out$tropicos)
#' }

get_ids <- function(names, db = c('itis','ncbi','eol','col','tropicos','gbif','ubio','nbn'), ...)
{
  if(is.null(db))
    stop("Must specify on or more values for db!")
  db <- match.arg(db, choices = c('itis','ncbi','eol','col','tropicos','gbif','ubio','nbn'),
                  several.ok = TRUE)

  foo <- function(x, names, ...){
    ids <- switch(x,
                  itis = get_tsn(names, ...),
                  ncbi = get_uid(names, ...),
                  eol = get_eolid(names, ...),
                  col = get_colid(names, ...),
                  tropicos = get_tpsid(names, ...),
                  gbif = get_gbifid(names, ...),
                  ubio = get_ubioid(names, ...),
                  nbn = get_nbnid(names, ...))
    names(ids) <- names
    return( ids )
  }

  tmp <- lapply(db, function(x) foo(x, names=names, ...))
  names(tmp) <- db
  class(tmp) <- "ids"
  return( tmp )
}
