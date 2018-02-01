#' Get scientific names from common names.
#'
#' @export
#' @param commnames One or more common names or partial names.
#' @param db Data source, one of \emph{"eol"} (default), \emph{"itis"},
#' \emph{"tropicos"}, \emph{"ncbi"}, or \emph{"worms"}.
#' @param itisby Search for common names across entire names (search, default),
#' at beginning of names (begin), or at end of names (end).
#' @param simplify (logical) If \code{TRUE}, simplify output to a vector
#' of names. If \code{FALSE}, return variable formats from different sources,
#' usually a data.frame.
#' @param ... Further arguments passed on to internal methods.
#' @return If \code{simplify=TRUE}, a list of scientific names, with list
#' labeled by your input names. If \code{simplify=FALSE}, a data.frame with
#' columns that vary by data source
#' @seealso \code{\link[taxize]{sci2comm}}
#' @details For data sources ITIS and NCBI you can pass in common names
#' directly, and use \code{\link[taxize]{get_uid}} or
#' \code{\link[taxize]{get_tsn}} to get ids first, then pass in to this fxn.
#'
#' For the other data sources, you can only pass in common names directly.
#' 
#' @section Authentication:
#' See \code{\link{taxize-authentication}} for help on authentication
#' 
#' @author Scott Chamberlain
#' @examples \dontrun{
#' comm2sci(commnames='black bear')
#' comm2sci(commnames='black bear', simplify = FALSE)
#' comm2sci(commnames='black bear', db='itis')
#' comm2sci(commnames='annual blue grass', db='tropicos')
#' comm2sci(commnames=c('annual blue grass','tree of heaven'), db='tropicos')
#' comm2sci(commnames=c('black bear', 'roe deer'))
#' comm2sci('blue whale', db = "worms")
#' comm2sci(c('blue whale', 'dwarf surfclam'), db = "worms")
#'
#' # Output easily converts to a data.frame with plyr::ldply
#' library(plyr)
#' ldply(comm2sci(commnames=c('annual blue grass','tree of heaven'),
#'   db='tropicos'))
#'
#' # ncbi: pass in uid's from get_uid() directly
#' x <- get_uid("western capercaillie", modifier = "Common Name")
#' comm2sci(x)
#' # itis: pass in tsn's from get_tsn() directly
#' x <- get_tsn(c("Louisiana black bear", "american crow"),
#'   searchtype = "common")
#' comm2sci(x)
#' }
comm2sci <- function(commnames, db='eol', itisby='search',
                     simplify=TRUE, ...) {
  UseMethod("comm2sci")
}

#' @export
comm2sci.default <- function(commnames, db='eol', itisby='search',
                             simplify=TRUE, ...) {
  assert(commnames, "character")
  assert(simplify, "logical")
  temp <- lapply(commnames, sci_from_comm, db = db, simplify = simplify,
                 itisby = itisby, ...)
  stats::setNames(temp, commnames)
}

sci_from_comm <- function(nn, db, simplify, itisby, ...) {
  switch(
    db,
    eol = c2s_eol(terms = nn, simplify, ...),
    itis = c2s_itis(nn, itisby, simplify, ...),
    tropicos = c2s_tp(simplify, commonname = nn, ...),
    ncbi = {
      ids <- get_uid(nn, modifier = "Common Name", ...)
      c2s_ncbi(ids, ...)
    },
    worms = c2s_worms(nn, simplify, ...),
    stop("'db' must be one of 'eol', 'itis', 'tropicos', 'ncbi', 'worms'",
         call. = FALSE)
  )
}

#' @export
comm2sci.tsn <- function(commnames, db='eol', itisby='search',
                         simplify=TRUE, ...) {
  temp <- lapply(commnames, c2s_itis_, simplify = simplify, ...)
  stats::setNames(temp, commnames)
}

#' @export
comm2sci.uid <- function(commnames, db='eol', itisby='search',
                         simplify=TRUE, ...) {
  temp <- lapply(commnames, c2s_ncbi, simplify = simplify, ...)
  stats::setNames(temp, commnames)
}

# helpers ------------
c2s_itis <- function(x, by='search', simplify, ...){
  tmp <- switch(
    by,
    search = ritis::search_common(x, ...),
    begin = ritis::search_common(x, from = "begin", ...),
    end = ritis::search_common(x, from = "end", ...)
  )
  # remove empty tsn slots
  tsns <- as.character(suppressWarnings(tmp$tsn))
  tsns <- tsns[!sapply(tsns, nchar, keepNA = FALSE) == 0]
  # get scientific names
  tmp <- data.table::setDF(
    data.table::rbindlist(
      lapply(tsns, ritis::scientific_name),
      fill = TRUE, use.names = TRUE
    )
  )
  if (simplify) {
    as.character(tmp$combinedname)
  } else{
    tmp
  }
}

c2s_itis_ <- function(x, by='search', simplify, ...){
  tmp <- data.table::setDF(
    data.table::rbindlist(
      lapply(x, ritis::scientific_name),
      fill = TRUE, use.names = TRUE
    )
  )
  if (simplify) {
    as.character(tmp$combinedname)
  } else{
    tmp
  }
}

c2s_ncbi <- function(x, simplify, ...) {
  key <- getkey(NULL, "ENTREZ_KEY")
  query <- list(db = "taxonomy", ID = x, api_key = key)
  cli <- crul::HttpClient$new(url = ncbi_base(), options = list(...))
  res <- cli$get("entrez/eutils/efetch.fcgi", query = query)
  res$raise_for_status()
  tt <- res$parse("UTF-8")
  ttp <- xml2::read_xml(tt)
  # common name
  out <- xml_text(xml_find_all(ttp, "//TaxaSet/Taxon/ScientificName"))
  # NCBI limits requests to three per second
  Sys.sleep(0.33)
  return(out)
}

c2s_eol <- function(simplify, ...){
  tmp <- eol_search(...)
  if (simplify) {
    as.character(tmp$name)
  } else {
    tmp
  }
}

c2s_tp <- function(simplify, ...){
  tmp <- tp_search(...)
  if (simplify) {
    as.character(tmp$scientificname)
  } else{
    tmp
  }
}

c2s_worms <- function(x, simplify, ...){
  tmp <- try_df(worrms::wm_records_common(name = x, ...))
  if (simplify) {
    as.character(tmp$scientificname)
  } else{
    tmp
  }
}
