#' Retrieve synonyms from various sources given input taxonomic names or identifiers.
#'
#' @param x character; taxons to query.
#' @param db character; database to query. either \code{itis}, \code{tropicos},
#' \code{ubio}, \code{col}, or \code{nbn}.
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
#' @details If IDs are supplied directly (not from the \code{get_*} functions) you
#' must specify the type of ID.
#'
#' For \code{db = "itis"} you can pass in a parameter \code{accepted} to toggle whether
#' only accepted names are used \code{accepted = TRUE}, or if all are used
#' \code{accepted = FALSE}. The default is \code{accepted = FALSE}.
#'
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{get_tpsid}},
#' \code{\link[taxize]{get_ubioid}}, \code{\link[taxize]{get_nbnid}}
#'
#' @export
#' @examples \dontrun{
#' # Plug in taxon names directly
#' synonyms("Pinus contorta", db="itis")
#' synonyms("Puma concolor", db="itis")
#' synonyms(c("Poa annua",'Pinus contorta','Puma concolor'), db="itis")
#' synonyms("Poa annua", db="tropicos")
#' synonyms("Pinus contorta", db="tropicos")
#' synonyms(c("Poa annua",'Pinus contorta'), db="tropicos")
#' synonyms("Salmo friderici", db='ubio')
#' synonyms(c("Salmo friderici",'Carcharodon carcharias','Puma concolor'), db="ubio")
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
#' synonyms(get_ubioid("Carcharodon carcharias"))
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
#' }

synonyms <- function(...) {
  UseMethod("synonyms")
}

#' @export
#' @rdname synonyms
synonyms.default <- function(x, db = NULL, rows = NA, ...) {
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
         col = {
           id <- get_colid(x, rows = rows, ...)
           setNames(synonyms(id, ...), x)
         },
         stop("the provided db value was not recognised", call. = FALSE)
  )
}

#' @export
#' @rdname synonyms
synonyms.tsn <- function(id, ...) {
  fun <- function(x){
    if (is.na(x)) { NA } else {
      is_acc <- getacceptednamesfromtsn(x, ...)
      if (is(is_acc, "list")) {
        x <- is_acc$acceptedTsn
        accdf <- setNames(data.frame(is_acc, stringsAsFactors = FALSE),
                          c("sub_tsn", "acc_name", "acc_tsn"))
        message("Accepted name is '", is_acc$acceptedName, "'")
        message("Using tsn ", is_acc$acceptedTsn, "\n")
      } else {
        accdf <- data.frame(sub_tsn = x, acc_tsn = x, stringsAsFactors = FALSE)
      }
      out <- setNames(getsynonymnamesfromtsn(x, ...), c('syn_name', 'syn_tsn'))
      if (as.character(out[1,1]) == 'nomatch') out <- data.frame(message = "no syns found", stringsAsFactors = FALSE)
#       if (is(is_acc, "list")) {
        cbind(accdf, out)
#       } else {
#         out
#       }
    }
  }
  setNames(lapply(id, fun), id)
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
  setNames(lapply(id, fun), id)
}

col_synonyms <- function(x, ...) {
  base <- "http://www.catalogueoflife.org/col/webservice"
  args <- list(id = x, response = "full")
  res <- GET(base, query = args)
  stop_for_status(res)
  out <- xmlParse(content(res, "text", encoding = "UTF-8"), encoding = "UTF-8")
  xml <- xpathApply(out, "//synonyms")
  if (length(xpathApply(xml[[1]], "synonym")) == 0) {
    NULL
  } else {
    nodes <- getNodeSet(xml[[1]], "//synonym")
    toget <- c("id", "name", "rank", "name_status", "genus", "species", "infraspecies", "author", "url")
    taxize_ldfast(lapply(nodes, function(z) {
      data.frame(sapply(toget, function(y) xpathApply(z, y, xmlValue)), stringsAsFactors = FALSE)
    }))
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
  setNames(lapply(id, fun), id)
}

#' @export
#' @rdname synonyms
synonyms.ubioid <- function(id, ...) {
  fun <- function(x){
    if (is.na(x)) {
      NA
    } else {
      ubio_id(namebankID = x, ...)[['synonyms']]
    }
  }
  setNames(lapply(id, fun), id)
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
  setNames(lapply(id, fun), id)
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
