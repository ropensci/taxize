#' Retrieve the downstream taxa for a given taxon name or ID.
#'
#' This function uses a while loop to continually collect children taxa down
#' to the taxonomic rank that you specify in the `downto` parameter. You
#' can get data from ITIS (itis), GBIF (gbif), NCBI (ncbi) or WORMS (worms).
#' There is no method exposed by these four 
#' services for getting taxa at a specific taxonomic rank, so we do it 
#' ourselves here.
#'
#' @export
#' @param x Vector of taxa names (character) or IDs (character or numeric)
#' to query.
#' @param db character; database to query. One or more of `itis`, `gbif`,
#' `ncbi` or `worms`. Note that each taxonomic  data source has their own
#' identifiers, so that if you provide the wrong `db` value for the identifier
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
#' @param limit Number of records to return
#' @param start Record number to start at
#' @param ... Further args passed on to [itis_downstream()],
#' [gbif_downstream()], [ncbi_downstream()],
#' or [worms_downstream()]
#'
#' @return A named list of data.frames with the downstream names of every
#' supplied taxa. You get an NA if there was no match in the database.
#' 
#' @section Authentication:
#' See [taxize-authentication] for help on authentication
#'
#' @examples \dontrun{
#' # Plug in taxon IDs
#' downstream(125732, db = 'worms', downto = 'species')
#'
#' # Plug in taxon names
#' downstream("Apis", db = 'ncbi', downto = 'species')
#' downstream("Apis", db = 'itis', downto = 'species')
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
#' }
downstream <- function(...){
  UseMethod("downstream")
}

#' @export
#' @rdname downstream
downstream.default <- function(x, db = NULL, downto = NULL,
                               intermediate = FALSE, rows=NA, ...) {
  nstop(downto, "downto")
  nstop(db)
  switch(
    db,
    itis = {
      id <- process_stream_ids(x, db, get_tsn, rows = rows, ...)
      stats::setNames(downstream(id, downto = tolower(downto),
                                 intermediate = intermediate, ...), x)
    },
    gbif = {
      id <- process_stream_ids(x, db, get_gbifid, rows = rows, ...)
      stats::setNames(downstream(id, downto = tolower(downto),
                                 intermediate = intermediate, ...), x)
    },
    ncbi = {
      id <- process_stream_ids(x, db, get_uid, rows = rows, ...)
      stats::setNames(downstream(id, downto = tolower(downto),
                                 intermediate = intermediate, ...), x)
    },
    worms = {
      id <- process_stream_ids(x, db, get_wormsid, rows = rows, ...)
      stats::setNames(downstream(id, downto = tolower(downto),
                                 intermediate = intermediate, ...), x)
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
      ncbi = as.uid, worms = as.wormsid)
    as_fxn(input, check = FALSE)
  } else {
    eval(fxn)(input, ...)
  }
}

#' @export
#' @rdname downstream
downstream.tsn <- function(x, db = NULL, downto = NULL,
                           intermediate = FALSE, ...) {
  warn_db(list(db = db), "itis")
  fun <- function(y, downto, intermediate, ...) {
    # return NA if NA is supplied
    if (is.na(y)) {
      NA
    } else {
		  itis_downstream(tsns = y, downto = downto,
		                  intermediate = intermediate, ...)
    }
  }
  out <- lapply(x, fun, downto = downto, intermediate = intermediate, ...)
  structure(out, class = 'downstream', db = 'itis', .Names = x)
}

#' @export
#' @rdname downstream
downstream.gbifid <- function(x, db = NULL, downto = NULL,
                              intermediate = FALSE, limit = 100,
                              start = NULL, ...) {
  warn_db(list(db = db), "gbif")
  fun <- function(y, downto, intermediate, limit, start, ...){
    # return NA if NA is supplied
    if (is.na(y)) {
      NA
    } else {
      gbif_downstream(key = y, downto = downto,
                      intermediate = intermediate, limit = limit,
                      start = start, ...)
    }
  }
  out <- lapply(x, fun, downto = downto, intermediate = intermediate,
    limit = limit, start = start, ...)
  structure(out, class = 'downstream', db = 'gbif')
}

#' @export
#' @rdname downstream
downstream.uid <- function(x, db = NULL, downto = NULL,
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
  out <- lapply(x, fun, downto = downto, intermediate = intermediate, ...)
  structure(out, class = 'downstream', db = 'ncbi')
}

#' @export
#' @rdname downstream
downstream.wormsid <- function(x, db = NULL, downto = NULL,
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
  out <- lapply(x, fun, downto = downto, intermediate = intermediate, ...)
  structure(out, class = 'downstream', db = 'worms')
}

#' @export
#' @rdname downstream
downstream.ids <- function(x, db = NULL, downto = NULL,
                           intermediate = FALSE, ...) {
  fun <- function(y, downto, intermediate, ...){
    # return NA if NA is supplied
    if (is.na(y)) {
      NA
    } else {
      downstream(y, downto = downto, intermediate = intermediate, ...)
    }
  }
  structure(lapply(x, fun, downto = downto, intermediate = intermediate, ...),
            class = 'downstream_ids')
}

simp <- function(x) if (length(x) == 1) x[[1]] else x
