#' Retrieve taxonomic identifiers for a given taxon name.
#' 
#' This is a convenience function to get identifiers across all data sources. You can 
#' use other \code{get_*} functions to get identifiers from specific sources if 
#' you like.
#' 
#' @export
#' @param names character; Taxonomic name to query.
#' @param db character; database to query. One or  more of \code{ncbi}, \code{itis}, 
#'    \code{eol}, \code{col}, \code{tropicos}, \code{gbif}, \code{worms}, \code{pesi},
#'    or \code{ubio}
#' @param ... Other arguments passed to \code{\link[taxize]{get_tsn}}, 
#'    \code{\link[taxize]{get_uid}}, \code{\link[taxize]{get_eolid}}, 
#'    \code{\link[taxize]{get_colid}}, \code{\link[taxize]{get_tpsid}},
#'    \code{\link[taxize]{get_gbifid}}, \code{\link[taxize]{get_wormsid}},
#'    \code{\link[taxize]{get_pesiid}}, or \code{\link[taxize]{get_ubioid}}.
#' @return A vector of taxonomic identifiers, each retaining their respective S3
#'    classes so that each element can be passed on to another function (see e.g.'s).
#' @note There is a timeout of 1/3 seconds between queries to NCBI.
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{get_uid}}, 
#'    \code{\link[taxize]{get_eolid}}, \code{\link[taxize]{get_colid}}, 
#'    \code{\link[taxize]{get_tpsid}}, \code{\link[taxize]{get_gbifid}}, or 
#'    \code{\link[taxize]{get_wormsid}}, \code{\link[taxize]{get_pesiid}}, or 
#'    \code{\link[taxize]{get_ubioid}}.
#' @examples \dontrun{
#' # Plug in taxon names directly
#' get_ids(names="Chironomus riparius", db = 'ncbi')
#' get_ids(names="Salvelinus fontinalis", db = 'worms')
#' get_ids(names="Salvelinus fontinalis", db = 'ubio')
#' get_ids(names="Salvelinus fontinalis", db = 'pesi')
#' get_ids(names="Salvelinus fontinalis", db = c('worms','ncbi'))
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

get_ids <- function(names, db = NULL, ...)
{
  if(is.null(db))
    stop("Must specify on or more values for db!")
  db <- match.arg(db, choices = c('itis','ncbi','eol','col','tropicos','gbif','worms','pesi','ubio'), several.ok = TRUE)
 
  foo <- function(x, names, ...){
    ids <- switch(x, 
                  itis = get_tsn(names, ...),
                  ncbi = get_uid(names, ...),
                  eol = get_eolid(names, ...),
                  col = get_colid(names, ...),
                  tropicos = get_tpsid(names, ...),
                  gbif = get_gbifid(names, ...),
                  worms = get_wormsid(names, ...),
                  ubio = get_ubioid(names, ...),
                  pesi = get_pesiid(names, ...))
    names(ids) <- names
    return( ids )
  }
  
  tmp <- lapply(db, function(x) foo(x, names=names, ...))
  names(tmp) <- db
  class(tmp) <- "ids"
  return( tmp )
}
