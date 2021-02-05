#' Retrieve the downstream taxa for a given taxon name or ID.
#'
#' This function uses a while loop to continually collect children taxa down
#' to the taxonomic rank that you specify in the `downto` parameter. You
#' can get data from ITIS (itis), GBIF (gbif), NCBI (ncbi), WORMS (worms),
#' or BOLD (bold). There is no method exposed by these four
#' services for getting taxa at a specific taxonomic rank, so we do it
#' ourselves here.
#'
#' @export
#' @param sci_id Vector of taxa names (character) or IDs (character or numeric)
#' to query.
#' @param db character; database to query. One or more of `itis`, `gbif`,
#' `ncbi`, `worms`, or `bold`. Note that each taxonomic  data source has
#' their own identifiers, so that if you provide the wrong `db` value for
#' the identifier
#' you could get a result, but it will likely be wrong (not what you were
#' expecting). If using ncbi, we recommend getting an API key; see
#' [taxize-authentication]
#' @param downto What taxonomic rank to go down to. One of: 'superkingdom',
#' 'kingdom', 'subkingdom','infrakingdom','phylum','division','subphylum',
#' 'subdivision','infradivision', 'superclass','class','subclass','infraclass',
#' 'superorder','order','suborder','infraorder','superfamily','family',
#' 'subfamily','tribe','subtribe','genus','subgenus','section','subsection',
#' 'species group','species','subspecies','variety','form','subvariety','race',
#' 'stirp', 'morph','aberration','subform', 'unspecified', 'no rank'
#' @param intermediate (logical) If `TRUE`, return a list of length two
#' with target taxon rank names, with additional list of data.frame's of
#' intermediate taxonomic groups. Default: `FALSE`
#' @param rows (numeric) Any number from 1 to infinity. If the default NA, all
#' rows are considered. Note that this parameter is ignored if you pass in a
#' taxonomic id of any of the acceptable classes: tsn.
#' @param limit Number of records to return. Applies to gbif only.
#' default: 100. max: 1000. use in combination with the `start` parameter
#' @param start Record number to start at. Applies to gbif only. default: 0.
#' use in combination with the `limit` parameter
#' @param x Deprecated, see `sci_id`
#' @param ... Further args passed on to [itis_downstream()],
#' [gbif_downstream()], [ncbi_downstream()],
#' [worms_downstream()], or [bold_downstream()]
#'
#' @return A named list of data.frames with the downstream names of every
#' supplied taxa. You get an NA if there was no match in the database.
#'
#' @section Authentication:
#' See [taxize-authentication] for help on authentication
#' 
#' @section bold:
#' BEWARE: `db="bold"` scrapes the BOLD website, so may be unstable. That is,
#' one day it may work, and the next it may fail. Open an issue if you
#' encounter an error: https://github.com/ropensci/taxize/issues
#'
#' @examples \dontrun{
#' # Plug in taxon IDs
#' downstream(125732, db = 'worms', downto = 'species')
#' downstream(3451, db = 'bold', downto = 'species')
#' 
#' if (interactive()) {
#'
#' # Plug in taxon names
#' downstream("Apis", db = 'ncbi', downto = 'species')
#' downstream("Apis", db = 'itis', downto = 'species')
#' downstream("Apis", db = 'bold', downto = 'species')
#' downstream("Gadus", db = 'worms', downto = 'species')
#' downstream(c("Apis","Epeoloides"), db = 'itis', downto = 'species')
#' downstream("Ursus", db = 'gbif', downto = 'species')
#' downstream(get_gbifid("Ursus"), db = 'gbif', downto = 'species')
#'
#' # Many taxa
#' sp <- names_list("genus", 3)
#' downstream(sp, db = 'itis', downto = 'species')
#' downstream(sp, db = 'gbif', downto = 'species')
#'
#' # Both data sources
#' ids <- get_ids("Apis", db = c('gbif','itis'))
#' downstream(ids, downto = 'species')
#' ## same result
#' downstream(get_ids("Apis", db = c('gbif','itis')), downto = 'species')
#'
#' # Collect intermediate names
#' ## itis
#' downstream('Bangiophyceae', db="itis", downto="genus")
#' downstream('Bangiophyceae', db="itis", downto="genus", intermediate=TRUE)
#' downstream(get_tsn('Bangiophyceae'), downto="genus")
#' downstream(get_tsn('Bangiophyceae'), downto="genus", intermediate=TRUE)
#'
#' # Use the rows parameter
#' ## note how in the second function call you don't get the prompt
#' downstream("Poa", db = 'gbif', downto="species")
#' downstream("Poa", db = 'gbif', downto="species", rows=1)
#'
#' # use curl options
#' res <- downstream("Apis", db = 'gbif', downto = 'species', verbose = TRUE)
#' 
#' # Pagination
#' # GBIF limits queries to a maximum of 1000 records per request, so if
#' # there's more than 1000, use the start parameter
#' # Piper, taxonKey = 3075433
#' z1 <- downstream(3075433, db = 'gbif', downto = "species", limit=1000)
#' z2 <- downstream(3075433, db = 'gbif', downto = "species", limit=1000,
#'   start=1000)
#' z3 <- downstream(3075433, db = 'gbif', downto = "species", limit=1000,
#'   start=2000)
#' z4 <- downstream(3075433, db = 'gbif', downto = "species", limit=1000,
#'   start=3000)
#' NROW(rbind(z1[[1]], z2[[1]], z3[[1]], z4[[1]]))
#' }}
downstream <- function(...){
  UseMethod("downstream")
}

#' @export
#' @rdname downstream
downstream.default <- function(sci_id, db=NULL, downto=NULL,
                               intermediate=FALSE, rows=NA, x=NULL, ...) {
  nstop(downto, "downto")
  nstop(db)
  pchk(x, "sci_id")
  if (!is.null(x)) sci_id <- x
  switch(
    db,
    itis = {
      id <- process_stream_ids(sci_id, db, get_tsn, rows = rows, ...)
      stats::setNames(downstream(id, downto = tolower(downto),
                                 intermediate = intermediate, ...), sci_id)
    },
    gbif = {
      id <- process_stream_ids(sci_id, db, get_gbifid, rows = rows, ...)
      stats::setNames(downstream(id, downto = tolower(downto),
                                 intermediate = intermediate, ...), sci_id)
    },
    ncbi = {
      id <- process_stream_ids(sci_id, db, get_uid, rows = rows, ...)
      stats::setNames(downstream(id, downto = tolower(downto),
                                 intermediate = intermediate, ...), sci_id)
    },
    worms = {
      id <- process_stream_ids(sci_id, db, get_wormsid, rows = rows, ...)
      stats::setNames(downstream(id, downto = tolower(downto),
                                 intermediate = intermediate, ...), sci_id)
    },
    bold = {
      id <- process_stream_ids(as.character(sci_id), db, get_boldid, rows = rows, ...)
      stats::setNames(downstream(id, downto = tolower(downto),
                                 intermediate = intermediate, ...), sci_id)
    },
    stop("the provided db value was not recognised/is not supported",
         call. = FALSE)
  )
}

process_stream_ids <- function(input, db, fxn, ...){
  g <- tryCatch(as.numeric(as.character(input)), warning = function(e) e)
  if (
    inherits(g, "numeric") ||
    is.character(input) &&
    all(grepl("[[:digit:]]", input))
  ) {
    as_fxn <- switch(db, itis = as.tsn, gbif = as.gbifid,
      ncbi = as.uid, worms = as.wormsid, bold = as.boldid)
    as_fxn(input, check = FALSE)
  } else {
    eval(fxn)(input, ...)
  }
}

#' @export
#' @rdname downstream
downstream.tsn <- function(sci_id, db = NULL, downto = NULL,
                           intermediate = FALSE, ...) {
  warn_db(list(db = db), "itis")
  fun <- function(y, downto, intermediate, ...) {
    # return NA if NA is supplied
    if (is.na(y)) {
      NA
    } else {
		  itis_downstream(y, downto = downto,
		                  intermediate = intermediate, ...)
    }
  }
  out <- lapply(sci_id, fun, downto = downto, intermediate = intermediate, ...)
  structure(out, class = 'downstream', db = 'itis', .Names = sci_id)
}

#' @export
#' @rdname downstream
downstream.gbifid <- function(sci_id, db = NULL, downto = NULL,
                              intermediate = FALSE, limit = 100,
                              start = NULL, ...) {
  warn_db(list(db = db), "gbif")
  fun <- function(y, downto, intermediate, limit, start, ...){
    # return NA if NA is supplied
    if (is.na(y)) {
      NA
    } else {
      gbif_downstream(y, downto = downto,
                      intermediate = intermediate, limit = limit,
                      start = start, ...)
    }
  }
  out <- lapply(sci_id, fun, downto = downto, intermediate = intermediate,
    limit = limit, start = start, ...)
  structure(out, class = 'downstream', db = 'gbif')
}

#' @export
#' @rdname downstream
downstream.uid <- function(sci_id, db = NULL, downto = NULL,
                              intermediate = FALSE, ...) {
  warn_db(list(db = db), "ncbi")
  fun <- function(y, downto, intermediate, ...){
    # return NA if NA is supplied
    if (is.na(y)) {
      NA
    } else {
      ncbi_downstream(id = y, downto = downto,
                      intermediate = intermediate, ...)
    }
  }
  out <- lapply(sci_id, fun, downto = downto, intermediate = intermediate, ...)
  structure(out, class = 'downstream', db = 'ncbi')
}

#' @export
#' @rdname downstream
downstream.wormsid <- function(sci_id, db = NULL, downto = NULL,
                              intermediate = FALSE, ...) {
  warn_db(list(db = db), "worms")
  fun <- function(y, downto, intermediate, ...){
    # return NA if NA is supplied
    if (is.na(y)) {
      NA
    } else {
      worms_downstream(id = y, downto = downto,
                      intermediate = intermediate, ...)
    }
  }
  out <- lapply(sci_id, fun, downto = downto, intermediate = intermediate, ...)
  structure(out, class = 'downstream', db = 'worms')
}

#' @export
#' @rdname downstream
downstream.boldid <- function(sci_id, db = NULL, downto = NULL,
                              intermediate = FALSE, ...) {
  warn_db(list(db = db), "bold")
  fun <- function(y, downto, intermediate, ...){
    # return NA if NA is supplied
    if (is.na(y)) {
      NA
    } else {
      bold_downstream(id = y, downto = downto,
                      intermediate = intermediate, ...)
    }
  }
  out <- lapply(sci_id, fun, downto = downto, intermediate = intermediate, ...)
  structure(out, class = 'downstream', db = 'bold')
}

#' @export
#' @rdname downstream
downstream.ids <- function(sci_id, db = NULL, downto = NULL,
                           intermediate = FALSE, ...) {
  fun <- function(y, downto, intermediate, ...){
    # return NA if NA is supplied
    if (is.na(y)) {
      NA
    } else {
      downstream(y, downto = downto, intermediate = intermediate, ...)
    }
  }
  structure(lapply(sci_id, fun, downto = downto, intermediate = intermediate, ...),
            class = 'downstream_ids')
}

simp <- function(x) if (length(x) == 1) x[[1]] else x
