#' Retrieve immediate children taxa for a given taxon name or ID.
#'
#' This function is different from [downstream()] in that it only
#' collects immediate taxonomic children, while [downstream()]
#' collects taxonomic names down to a specified taxonomic rank, e.g.,
#' getting all species in a family.
#'
#' @export
#' @param sci_id Vector of taxa names (character) or IDs (character or numeric)
#' to query.
#' @param db character; database to query. One or more of `itis`,
#' `ncbi`, `worms`, or `bold`. Note that each taxonomic data
#' source has their own identifiers, so that if you provide the wrong
#' `db` value for the identifier you could get a result, but it will
#' likely be wrong (not what you were expecting). If using ncbi, we recommend
#' getting an API key; see [taxize-authentication]
#' @param rows (numeric) Any number from 1 to infinity. If the default NA, all
#' rows are considered. Note that this parameter is ignored if you pass in a
#' taxonomic id of any of the acceptable classes: itis. NCBI has a
#' method for this function but rows doesn't work.
#' @param x Deprecated, see `sci_id`
#' @param ... Further args passed on to [ritis::hierarchy_down()],
#' [ncbi_children()], [worrms::wm_children()], [bold_children()]
#' See those functions for what parameters can be passed on.
#'
#' @section ncbi:
#' note that with `db = "ncbi"`, we set `ambiguous = TRUE`; that is, children
#' taxa with words like "unclassified", "unknown", "uncultured", "sp." are
#' NOT removed
#' 
#' @section bold:
#' BEWARE: `db="bold"` scrapes the BOLD website, so may be unstable. That is,
#' one day it may work, and the next it may fail. Open an issue if you
#' encounter an error: https://github.com/ropensci/taxize/issues
#'
#' @return A named list of data.frames with the children names of every
#' supplied taxa. You get an NA if there was no match in the database.
#'
#' @examples \dontrun{
#' # Plug in taxonomic IDs
#' children(161994, db = "itis")
#' children(8028, db = "ncbi")
#' ## works with numeric if as character as well
#' children(161994, db = "itis")
#' children(88899, db = "bold")
#' children(as.boldid(88899))
#'
#' # Plug in taxon names
#' children("Salmo", db = 'itis')
#' children("Salmo", db = 'ncbi')
#' children("Salmo", db = 'worms')
#' children("Salmo", db = 'bold')
#'
#' # Plug in IDs
#' (id <- get_worms("Gadus"))
#' children(id)
#' (z <- get_itis("Tragia"))
#' children(z)
#'
#' # Many taxa
#' sp <- c("Tragia", "Schistocarpha", "Encalypta")
#' children(sp, db = 'itis')
#'
#' # Two data sources
#' (ids <- get_ids("Apis", db = c('ncbi','itis')))
#' children(ids)
#' ## same result
#' children(get_ids("Apis", db = c('ncbi','itis')))
#'
#' # Use the rows parameter
#' children("Poa", db = 'itis')
#' children("Poa", db = 'itis', rows=1)
#'
#' # use curl options
#' res <- children("Poa", db = 'itis', rows=1, verbose = TRUE)
#' }

children <- function(...){
  UseMethod("children")
}

#' @export
#' @rdname children
children.default <- function(sci_id, db = NULL, rows = NA, x = NULL, ...) {
  nstop(db)
  pchk(x, "sci_id")
  if (!is.null(x)) sci_id <- x
  results <- switch(
    db,
    itis = {
      id <- process_children_ids(sci_id, db, get_itis, rows = rows, ...)
      stats::setNames(children(id, ...), sci_id)
    },

    ncbi = {
      if (all(grepl("^[[:digit:]]*$", sci_id))) {
        id <- sci_id
        class(id) <- "uid"
        stats::setNames(children(id, ...), sci_id)
      } else {
        out <- ncbi_children(name = sci_id, ...)
        structure(out, class = 'children', db = 'ncbi', .Names = sci_id)
      }
    },

    worms = {
      id <- process_children_ids(sci_id, db, get_worms, rows = rows, ...)
      stats::setNames(children(id, ...), sci_id)
    },

    bold = {
      id <- process_children_ids(as.character(sci_id), db, get_bold,
        rows = rows, ...)
      stats::setNames(children(id, ...), sci_id)
    },

    stop("the provided db value was not recognised", call. = FALSE)
  )

  set_output_types(results, sci_id, db)
}

# Ensure that the output types are consistent when searches return nothing
itis_blank <- data.frame(
  parentname = character(0),
  parenttsn  = character(0),
  rankname   = character(0),
  taxonname  = character(0),
  tsn        = character(0),
  stringsAsFactors = FALSE
)
worms_blank <- ncbi_blank <- bold_blank <-
  data.frame(
    childtaxa_id     = character(0),
    childtaxa_name   = character(0),
    childtaxa_rank   = character(0),
    stringsAsFactors = FALSE
  )

set_output_types <- function(x, x_names, db){
  blank_fun <- switch(
    db,
    itis  = function(w) if (nrow(w) == 0 || all(is.na(w))) itis_blank else w,
    ncbi  = function(w) if (nrow(w) == 0 || all(is.na(w))) ncbi_blank else w,
    worms = function(w) if (nrow(w) == 0 || all(is.na(w))) worms_blank else w,
    bold = function(w) if (nrow(w) == 0 || all(is.na(w))) bold_blank else w
  )

  typed_results <- lapply(seq_along(x), function(i) blank_fun(x[[i]]))
  names(typed_results) <- x_names
  attributes(typed_results) <- attributes(x)
  typed_results
}

process_children_ids <- function(input, db, fxn, ...){
  g <- tryCatch(as.numeric(as.character(input)), warning = function(e) e)
  if (inherits(g, "condition")) return(eval(fxn)(input, ...))
  if (is.numeric(g) || is.character(input) && all(grepl("[[:digit:]]", input))) {
    as_fxn <- switch(db, itis = as.itis, worms = as.worms, bold = as.bold)
    as_fxn(input, check = FALSE)
  } else {
    eval(fxn)(input, ...)
  }
}

#' @export
#' @rdname children
children.ids <- function(sci_id, db = NULL, ...) {
  fun <- function(y, ...){
    # return NA if NA is supplied
    if (is.na(y)) {
      out <- NA
    } else {
      out <- children(y, ...)
    }
    return(out)
  }
  out <- lapply(sci_id, fun)
  class(out) <- 'children_ids'
  return(out)
}

#' @export
#' @rdname children
children.txid <- function(sci_id, db = NULL, ...) {
  eval(parse(text=paste0("children_", id_class(sci_id))))(sci_id, db)
}

id_class <- function(x) {
  clazess <- c("worms", "uid", "itis", "pow", "gbif",
    "iucn", "natserv", "bold", "eol", "nbn", "tol", "tps")
  clazess[clazess %in% class(x)]
}

children_itis <- function(sci_id, db = NULL, ...) {
  warn_db(list(db = db), "itis")
  fun <- function(y){
    # return NA if NA is supplied
    if (is.na(y)) {
      out <- NA
    } else {
		  out <- ritis::hierarchy_down(y, ...)
    }
  }
  ids <- as.character(taxa::tax_id(sci_id))
  out <- lapply(ids, fun)
  names(out) <- taxa::tax_name(sci_id)
  class(out) <- 'children'
  attr(out, 'db') <- 'itis'
  return(out)
}

children_worms <- function(sci_id, db = NULL, ...) {
  warn_db(list(db = db), "worms")
  fun <- function(y){
    # return NA if NA is supplied
    if (is.na(y)) {
      out <- NA
    } else {
      out <- worrms::wm_children(as.numeric(y))
      out <- list(out)
      i <- 1
      while (NROW(out[[length(out)]]) == 50) {
        i <- i + 1
        out[[i]] <- worrms::wm_children(
          as.numeric(y),
          offset = sum(unlist(sapply(out, NROW))))
      }
      out <- df2dt2tbl(out)
      stats::setNames(
        out[names(out) %in% c('AphiaID', 'scientificname', 'rank')],
        c('childtaxa_id', 'childtaxa_name', 'childtaxa_rank')
      )
    }
  }
  ids <- as.character(taxa::tax_id(sci_id))
  out <- lapply(ids, fun)
  names(out) <- taxa::tax_name(sci_id)
  class(out) <- 'children'
  attr(out, 'db') <- 'worms'
  return(out)
}

children_uid <- function(sci_id, db = NULL, ...) {
  warn_db(list(db = db), "uid")
  sci_id <- as.character(taxa::tax_id(sci_id))
  out <- if (is.na(sci_id)) {
    stats::setNames(list(ncbi_blank), sci_id)
  } else {
    ncbi_children(id = sci_id, ambiguous = TRUE, ...)
  }
  class(out) <- 'children'
  attr(out, 'db') <- 'ncbi'
  return(out)
}

children_bold <- function(sci_id, db = NULL, ...) {
  warn_db(list(db = db), "bold")
  sci_id <- as.character(taxa::tax_id(sci_id))
  out <- if (is.na(sci_id)) {
    stats::setNames(list(bold_blank), sci_id)
  } else {
    bold_children(id = sci_id, ...)
  }
  class(out) <- 'children'
  attr(out, 'db') <- 'bold'
  return(out)
}
