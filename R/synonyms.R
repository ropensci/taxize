#' Retrieve synonyms from various sources given input taxonomic
#' names or identifiers
#'
#' @param x Vector of taxa names (character) or IDs (character or numeric) to
#' query.
#' @param db character; database to query. either \code{itis}, \code{tropicos},
#' \code{col}, or \code{nbn}. Note that each taxonomic data source has their
#' own identifiers, so that if you provide the wrong \code{db} value for the
#' identifier you could get a result, but it will likely be wrong (not what
#' you were expecting).
#' @param id character; identifiers, returned by \code{\link[taxize]{get_tsn}},
#' \code{\link[taxize]{get_tpsid}}, or \code{\link[taxize]{get_nbnid}}
#' @param rows (numeric) Any number from 1 to infinity. If the default NA, all
#' rows are considered. Note that this parameter is ignored if you pass in a
#' taxonomic id of any of the acceptable classes: tsn, tpsid, nbnid, ids.
#' @param ... Other passed arguments to internal functions \code{get_*()} and
#' functions to gather synonyms.
#'
#' @return A named list of data.frames with the synonyms of every supplied taxa.
#' @details If IDs are supplied directly (not from the \code{get_*} functions)
#' you must specify the type of ID.
#'
#' For \code{db = "itis"} you can pass in a parameter \code{accepted} to
#' toggle whether only accepted names are used \code{accepted = TRUE}, or if
#' all are used \code{accepted = FALSE}. The default is \code{accepted = FALSE}
#'
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{get_tpsid}},
#' \code{\link[taxize]{get_nbnid}}
#'
#' @export
#' @examples \dontrun{
#' # Plug in taxon IDs
#' synonyms(183327, db="itis")
#' synonyms("25509881", db="tropicos")
#' synonyms("NBNSYS0000004629", db='nbn')
#' synonyms("87e986b0873f648711900866fa8abde7", db='col')
#'
#' # Plug in taxon names directly
#' synonyms("Pinus contorta", db="itis")
#' synonyms("Puma concolor", db="itis")
#' synonyms(c("Poa annua",'Pinus contorta','Puma concolor'), db="itis")
#' synonyms("Poa annua", db="tropicos")
#' synonyms("Pinus contorta", db="tropicos")do
#' synonyms(c("Poa annua",'Pinus contorta'), db="tropicos")
#' synonyms("Pinus sylvestris", db='nbn')
#' synonyms("Puma concolor", db='col')
#' synonyms("Ursus americanus", db='col')
#' synonyms("Amblyomma rotundatum", db='col')
#'
#' # not accepted names, with ITIS
#' ## looks for whether the name given is an accepted name,
#' ## and if not, uses the accepted name to look for synonyms
#' synonyms("Acer drummondii", db="itis")
#' synonyms("Spinus pinus", db="itis")
#'
#' # Use get_* methods
#' synonyms(get_tsn("Poa annua"))
#' synonyms(get_tpsid("Poa annua"))
#' synonyms(get_nbnid("Carcharodon carcharias"))
#' synonyms(get_colid("Ornithodoros lagophilus"))
#'
#' # Pass many ids from class "ids"
#' out <- get_ids(names="Poa annua", db = c('itis','tropicos'))
#' synonyms(out)
#'
#' # Use the rows parameter to select certain rows
#' synonyms("Poa annua", db='tropicos', rows=1)
#' synonyms("Poa annua", db='tropicos', rows=1:3)
#' synonyms("Pinus sylvestris", db='nbn', rows=1:3)
#' synonyms("Amblyomma rotundatum", db='col', rows=2)
#' synonyms("Amblyomma rotundatum", db='col', rows=2:3)
#'
#' # Use curl options
#' synonyms("Poa annua", db='tropicos', rows=1, config=verbose())
#' synonyms("Poa annua", db='itis', rows=1, config=verbose())
#' synonyms("Poa annua", db='col', rows=1, config=verbose())
#'
#'
#' # combine many outputs together
#' x <- synonyms(c("Osmia bicornis", "Osmia rufa", "Osmia"), db = "itis")
#' synonyms_df(x)
#'
#' ## note here how Pinus contorta is dropped due to no synonyms found
#' x <- synonyms(c("Poa annua",'Pinus contorta','Puma concolor'), db="col")
#' synonyms_df(x)
#'
#' ## note here that ids are taxon identifiers b/c you start with them
#' x <- synonyms(c(25509881, 13100094), db="tropicos")
#' synonyms_df(x)
#'
#' ## xxx
#' x <- synonyms(c('Aglais io', 'Usnea hirta', 'Arctostaphylos uva-ursi'), db="nbn")
#' synonyms_df(x)
#' }

synonyms <- function(...) {
  UseMethod("synonyms")
}

#' @export
#' @rdname synonyms
synonyms.default <- function(x, db = NULL, rows = NA, ...) {
  nstop(db)
  switch(
    db,
    itis = {
      id <- process_syn_ids(x, db, get_tsn, rows = rows, ...)
      structure(stats::setNames(synonyms(id, ...), x),
                class = "synonyms", db = "itis")
    },
    tropicos = {
      id <- process_syn_ids(x, db, get_tpsid, rows = rows, ...)
      structure(stats::setNames(synonyms(id, ...), x),
                class = "synonyms", db = "tropicos")
    },
    nbn = {
      id <- process_syn_ids(x, db, get_nbnid, rows = rows, ...)
      structure(stats::setNames(synonyms(id, ...), x),
                class = "synonyms", db = "nbn")
    },
    col = {
      id <- process_syn_ids(x, db, get_colid, rows = rows, ...)
      structure(stats::setNames(synonyms(id, ...), x),
                class = "synonyms", db = "col")
    },
    stop("the provided db value was not recognised", call. = FALSE)
  )
}

process_syn_ids <- function(input, db, fxn, ...){
  g <- tryCatch(as.numeric(as.character(input)), warning = function(e) e)
  if (inherits(g,"numeric") || is.character(input) && grepl("N[HB]", input) ||
      is.character(input) && grepl("[[:digit:]]", input)) {
    as_fxn <- switch(db,
                     itis = as.tsn,
                     tropicos = as.tpsid,
                     nbn = as.nbnid,
                     col = as.colid)
    as_fxn(input, check = FALSE)
  } else {
    eval(fxn)(input, ...)
  }
}

#' @export
#' @rdname synonyms
synonyms.tsn <- function(id, ...) {
  fun <- function(x){
    if (is.na(x)) { NA } else {
      is_acc <- rit_acc_name(x, ...)
      if (!is.na(is_acc$acceptedName)) {
        x <- is_acc$acceptedTsn
        accdf <- setNames(
          data.frame(x[1], is_acc, stringsAsFactors = FALSE),
          c("sub_tsn", "acc_name", "acc_tsn", "author")
        )
        message("Accepted name is '", is_acc$acceptedName, "'")
        message("Using tsn ", is_acc$acceptedTsn, "\n")
      } else {
        accdf <- data.frame(sub_tsn = x[1], acc_tsn = x[1],
                            stringsAsFactors = FALSE)
      }
      out <- ritis::synonym_names(x, ...)
      if (NROW(out) == 0) {
        out <- data.frame(syn_name = "nomatch", syn_tsn = x[1],
                          stringsAsFactors = FALSE)
      } else {
        out <- setNames(out, c('author', 'syn_name', 'syn_tsn'))
      }
      if (as.character(out[1,1]) == 'nomatch') {
        out <- data.frame(message = "no syns found", stringsAsFactors = FALSE)
      }
      cbind(accdf, out)
    }
  }
  stats::setNames(lapply(id, fun), id)
}

rit_acc_name <- function(x, ...) {
  tmp <- ritis::accepted_names(x, ...)
  if (NROW(tmp) == 0) {
    data.frame(submittedtsn = x[1], acceptedName = NA, acceptedTsn = x[1],
               stringsAsFactors = FALSE)
  } else {
    tmp
  }
}

#' @export
#' @rdname synonyms
synonyms.colid <- function(id, ...) {
  fun <- function(x) {
    if (is.na(x)) {
      NA
    } else {
      col_synonyms(x, ...)
    }
  }
  stats::setNames(lapply(id, fun), id)
}

col_synonyms <- function(x, ...) {
  base <- "http://www.catalogueoflife.org/col/webservice"
  args <- list(id = x[1], response = "full", format = "json")
  res <- httr::GET(base, query = args, ...)
  httr::stop_for_status(res)
  out <- jsonlite::fromJSON(con_utf8(res), FALSE)
  tmp <- out$results[[1]]
  if ("synonyms" %in% names(tmp)) {
    df <- taxize_ldfast(lapply(tmp$synonyms, function(w) {
      w[sapply(w, length) == 0] <- NA
      w$references <- NULL
      data.frame(w, stringsAsFactors = FALSE)
    }))
    df$rank <- tolower(df$rank)

    df
  } else {
    NULL
  }
}

#' @export
#' @rdname synonyms
synonyms.tpsid <- function(id, ...) {
  fun <- function(x) {
    if (is.na(x)) {
      NA
    } else {
      tp_synonyms(x, ...)$synonyms
    }
  }
  stats::setNames(lapply(id, fun), id)
}

#' @export
#' @rdname synonyms
synonyms.nbnid <- function(id, ...) {
  fun <- function(x){
    if (is.na(x)) {
      NA
    } else {
      nbn_synonyms(x, ...)
    }
  }
  stats::setNames(lapply(id, fun), id)
}

#' @export
#' @rdname synonyms
synonyms.ids <- function(id, ...) {
  fun <- function(x){
    if (is.na(x)) {
      out <- NA
    } else {
      out <- synonyms(x, ...)
    }
    return( out )
  }
  lapply(id, fun)
}

### Combine synonyms output into single data.frame -----------
#' @export
#' @rdname synonyms
synonyms_df <- function(x) {
  UseMethod("synonyms_df")
}

#' @export
synonyms_df.default <- function(x) {
  stop("no 'synonyms_df' method for ", class(x), call. = FALSE)
}

#' @export
synonyms_df.synonyms <- function(x) {
  (data.table::setDF(
    data.table::rbindlist(x, use.names = TRUE, fill = TRUE, idcol = TRUE)
  ))
}
