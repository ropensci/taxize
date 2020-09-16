#' Retrieve synonyms from various sources given input taxonomic
#' names or identifiers
#'
#' @param sci_id Vector of taxa names (character) or IDs (character or numeric)
#' @param db character; database to query. either `itis`, `tropicos`,
#' `nbn`, `worms`, or `pow`. Note that each taxonomic data source has their own
#' identifiers, so that if you provide the wrong `db` value for the identifier
#' you could get a result, but it will likely be wrong (not what you were
#' expecting). If using tropicos, we  recommend getting an API key;
#' see [taxize-authentication]
#' @param id character; identifiers, returned by [get_itis()], [get_tps()],
#' [get_nbn()], [get_worms()], [get_pow()]
#' @param rows (numeric) Any number from 1 to infinity. If the default NA, all
#' rows are considered. Note that this parameter is ignored if you pass in a
#' taxonomic id of any of the acceptable classes: tsn, tpsid, nbnid, ids.
#' @param x For `synonyms()`: deprecated, see `sci_id`. For `synonyms_df()`, 
#' the output of `synonyms()`
#' @param ... Other passed arguments to internal functions `get_*()` and
#' functions to gather synonyms.
#'
#' @return A named list of results with three types of output in each slot:
#'
#' - if the name was not found: `NA_character_`
#' - if the name was found but no synonyms found, an empty data.frame (0 rows)
#' - if the name was found, and synonyms found, a data.frames with the
#' synonyms - the column names vary by data source
#'
#' @details If IDs are supplied directly (not from the `get_*()` functions)
#' you must specify the type of ID.
#'
#' For `db = "itis"` you can pass in a parameter `accepted` to
#' toggle whether only accepted names are used `accepted = TRUE`, or if
#' all are used `accepted = FALSE`. The default is `accepted = FALSE`
#'
#' Note that IUCN requires an API key. See [rredlist::rredlist-package]
#' for help on authentiating with IUCN Redlist
#'
#' @seealso [get_itis()] [get_tps()] [get_nbn()] [get_worms()] [get_iucn()]
#' [get_pow()]
#'
#' @export
#' @examples \dontrun{
#' # Plug in taxon IDs
#' synonyms(183327, db="itis")
#' synonyms("25509881", db="tropicos")
#' synonyms("NBNSYS0000004629", db='nbn')
#' synonyms(105706, db='worms')
#' synonyms(12392, db='iucn')
#' synonyms('urn:lsid:ipni.org:names:358881-1', db='pow')
#'
#' # Plug in taxon names directly
#' synonyms("Pinus contorta", db="itis")
#' synonyms("Puma concolor", db="itis")
#' synonyms(c("Poa annua",'Pinus contorta','Puma concolor'), db="itis")
#' synonyms("Poa annua", db="tropicos")
#' synonyms("Pinus contorta", db="tropicos")
#' synonyms(c("Poa annua",'Pinus contorta'), db="tropicos")
#' synonyms("Pinus sylvestris", db='nbn')
#' synonyms('Pomatomus', db='worms')
#' synonyms('Pomatomus saltatrix', db='worms')
#' synonyms('Lithocarpus mindanaensis', db='pow')
#' synonyms('Poa annua', db='pow')
#' synonyms(c('Poa annua', 'Pinus contorta', 'foo bar'), db='pow')
#'
#' # not accepted names, with ITIS
#' ## looks for whether the name given is an accepted name,
#' ## and if not, uses the accepted name to look for synonyms
#' synonyms("Acer drummondii", db="itis")
#' synonyms("Spinus pinus", db="itis")
#'
#' # Use get_* methods
#' synonyms(get_itis("Poa annua"))
#' synonyms(get_tps("Poa annua"))
#' synonyms(get_nbn("Carcharodon carcharias"))
#' synonyms(get_iucn('Loxodonta africana'))
#' synonyms(get_pow('Lithocarpus mindanaensis'))
#'
#' # Pass many ids from class "ids"
#' out <- get_ids("Poa annua", db = c('itis','tropicos'))
#' synonyms(out)
#'
#' # Use the rows parameter to select certain rows
#' synonyms("Poa annua", db='tropicos', rows=1)
#' synonyms("Poa annua", db='tropicos', rows=1:3)
#' synonyms("Pinus sylvestris", db='nbn', rows=1:3)
#'
#' # Use curl options
#' synonyms("Poa annua", db='tropicos', rows=1, verbose = TRUE)
#' synonyms("Poa annua", db='itis', rows=1, verbose = TRUE)
#'
#'
#' # combine many outputs together
#' x <- synonyms(c("Osmia bicornis", "Osmia rufa", "Osmia"), db = "itis")
#' synonyms_df(x)
#'
#' ## note here how Pinus contorta is dropped due to no synonyms found
#' synonyms_df(x)
#'
#' ## note here that ids are taxon identifiers b/c you start with them
#' x <- synonyms(c(25509881, 13100094), db="tropicos")
#' synonyms_df(x)
#'
#' ## NBN
#' x <- synonyms(c('Aglais io', 'Usnea hirta', 'Arctostaphylos uva-ursi'),
#'   db="nbn")
#' synonyms_df(x)
#' }

synonyms <- function(...) {
  UseMethod("synonyms")
}

#' @export
#' @rdname synonyms
synonyms.default <- function(sci_id, db = NULL, rows = NA, x = NULL, ...) {
  nstop(db)
  pchk(x, "sci")
  if (!is.null(x)) sci_id <- x
  switch(
    db,
    itis = {
      id <- process_syn_ids(sci_id, db, get_itis, rows = rows, ...)
      structure(stats::setNames(synonyms(id, ...), names_or_ids(id)),
                class = "synonyms", db = "itis")
    },
    tropicos = {
      id <- process_syn_ids(sci_id, db, get_tps, rows = rows, ...)
      structure(stats::setNames(synonyms(id, ...), names_or_ids(id)),
                class = "synonyms", db = "tropicos")
    },
    nbn = {
      id <- process_syn_ids(sci_id, db, get_nbn, rows = rows, ...)
      structure(stats::setNames(synonyms(id, ...), names_or_ids(id)),
                class = "synonyms", db = "nbn")
    },
    worms = {
      id <- process_syn_ids(sci_id, db, get_worms, rows = rows, ...)
      structure(stats::setNames(synonyms(id, ...), names_or_ids(id)),
                class = "synonyms", db = "worms")
    },
    iucn = {
      id <- process_syn_ids(as.character(sci_id), db, get_iucn, ...)
      structure(stats::setNames(synonyms(id, ...), names_or_ids(id)),
                class = "synonyms", db = "iucn")
    },
    pow = {
      id <- process_syn_ids(sci_id, db, get_pow, ...)
      structure(stats::setNames(synonyms(id, ...), names_or_ids(id)),
                class = "synonyms", db = "pow")
    },
    stop("the provided db value was not recognised", call. = FALSE)
  )
}

process_syn_ids <- function(input, db, fxn, ...){
  g <- tryCatch(as.numeric(as.character(input)), warning = function(e) e)
  if (inherits(g, "condition") && all(!grepl("ipni\\.org", input))) {
    return(eval(fxn)(input, ...))
  }
  if (
    is.numeric(g) ||
    is.character(input) && all(grepl("N[HB]", input)) ||
    is.character(input) && all(grepl("ipni\\.org", input)) ||
    is.character(input) && all(grepl("[[:digit:]]", input))
  ) {
    as_fxn <- switch(db,
      itis = as.itis,
      tropicos = as.tps,
      nbn = as.nbn,
      worms = as.worms,
      iucn = as.iucn,
      pow = as.pow)
    if (db == "iucn") return(as_fxn(input, check = TRUE))
    return(as_fxn(input, check = FALSE))
  } else {
    eval(fxn)(input, ...)
  }
}

#' @export
#' @rdname synonyms
synonyms.txid <- function(sci_id, db = NULL, ...) {
  db <- txdbac(sci_id)[1]
  fun <- parse(text=paste0("synonyms_", id_class(sci_id)))
  eval(fun)(sci_id, ...)
}

synonyms_itis <- function(id, ...) {
  warn_db(list(...), "itis")
  fun <- function(x){
    if (is.na(x)) { NA_character_ } else {
      is_acc <- rit_acc_name(x)
      if (all(!is.na(is_acc$acceptedName))) {
        accdf <- stats::setNames(
          data.frame(x[1], is_acc, stringsAsFactors = FALSE),
          c("sub_tsn", "acc_name", "acc_tsn", "acc_author")
        )
        x <- is_acc$acceptedTsn
        message("Accepted name(s) is/are '",
                paste0(is_acc$acceptedName, collapse = "/"), "'")
        message("Using tsn(s) ", paste0(is_acc$acceptedTsn, collapse = "/"),
                "\n")
      } else {
        accdf <- data.frame(sub_tsn = x[1], acc_tsn = x[1],
                            stringsAsFactors = FALSE)
      }

      res <- Map(function(z, w) {
        tmp <- ritis::synonym_names(z)
        if (NROW(tmp) == 0) {
          tibble::tibble()
        } else {
          tmp <- stats::setNames(tmp, c('syn_author', 'syn_name', 'syn_tsn'))
          cbind(w, tmp, row.names = NULL)
        }
      }, x, split(accdf, seq_len(NROW(accdf))))
      do.call("rbind", unname(res))
    }
  }
  stats::setNames(lapply(txidac(id), fun), names_or_ids(id))
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

synonyms_tps <- function(id, ...) {
  warn_db(list(...), "topicos")
  fun <- function(x) {
    if (is.na(x)) {
      NA_character_
    } else {
      res <- tp_synonyms(x, ...)$synonyms
      if (grepl("no syns found", res[1,1])) tibble::tibble() else res
    }
  }
  stats::setNames(lapply(txidac(id), fun), names_or_ids(id))
}

synonyms_nbn <- function(id, ...) {
  warn_db(list(...), "nbn")
  fun <- function(x){
    if (is.na(x)) {
      NA_character_
    } else {
      res <- nbn_synonyms(x, ...)
      if (length(res) == 0) tibble::tibble() else res
    }
  }
  stats::setNames(lapply(txidac(id), fun), names_or_ids(id))
}

synonyms_worms <- function(id, ...) {
  warn_db(list(...), "worms")
  fun <- function(x) {
    if (is.na(x)) {
      NA_character_
    } else {
      res <- tryCatch(worrms::wm_synonyms(as.numeric(x), ...),
        error = function(e) e)
      if (inherits(res, "error")) tibble::tibble() else res
    }
  }
  stats::setNames(lapply(txidac(id), fun), names_or_ids(id))
}

synonyms_iucn <- function(id, ...) {
  warn_db(list(...), "iucn")
  xid <- txidac(id)
  out <- vector(mode = "list", length = length(xid))
  for (i in seq_along(xid)) {
    if (is.na(xid[[i]])) {
      out[[i]] <- NA_character_
    } else {
      res <- rredlist::rl_synonyms(txnameac(id)[i], ...)$result
      out[[i]] <- if (length(res) == 0) tibble::tibble() else res
    }
  }
  stats::setNames(out, names_or_ids(id))
}

synonyms_pow <- function(id, ...) {
  warn_db(list(...), "pow")
  xid <- txidac(id)
  out <- vector(mode = "list", length = length(xid))
  for (i in seq_along(xid)) {
    if (is.na(xid[[i]])) {
      out[[i]] <- NA_character_
    } else {
      res <- pow_synonyms(xid[i], ...)
      out[[i]] <- if (length(res) == 0) {
        tibble::tibble() 
      } else {
        names(res)[1] <- "id"
        res
      }
    }
  }
  stats::setNames(out, names_or_ids(id))
}




#' @export
#' @rdname synonyms
synonyms.ids <- function(id, ...) {
  fun <- function(x){
    if (is.na(x)) {
      out <- NA_character_
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
  x <- Filter(function(z) inherits(z, "data.frame"), x)
  x <- Filter(function(z) NROW(z) > 0, x)
  (data.table::setDF(
    data.table::rbindlist(x, use.names = TRUE, fill = TRUE, idcol = TRUE)
  ))
}
