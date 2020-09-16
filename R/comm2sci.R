#' Get scientific names from common names.
#'
#' @export
#' @param com One or more common names or partial names.
#' @param db Data source, one of *"ncbi"* (default), *"itis"*,
#' *"tropicos"*, *"eol"*, or *"worms"*. If using ncbi, we
#' recommend getting an API key; see [taxize-authentication]
#' @param itisby Search for common names across entire names (search, default),
#' at beginning of names (begin), or at end of names (end).
#' @param simplify (logical) If `TRUE`, simplify output to a vector of names.
#' If `FALSE`, return variable formats from different sources, usually a
#' data.frame.
#' @param commnames Deprecated, see `com`
#' @param ... Further arguments passed on to internal methods.
#' @param id taxon identifiers, as returned by [get_itis()] or [get_ncbi()]
#' @return If `simplify=TRUE`, a list of scientific names, with list
#' labeled by your input names. If `simplify=FALSE`, a data.frame with
#' columns that vary by data source. `character(0)` on no match
#' @seealso [sci2comm()]
#' @details For data sources ITIS and NCBI you can pass in common names
#' directly, and use [get_ncbi()] or [get_itis()] to get ids first, then pass in
#' to this fxn.
#'
#' For the other data sources, you can only pass in common names directly.
#'
#' @section Authentication:
#' See [taxize-authentication] for help on authentication
#' 
#' @section HTTP version for NCBI requests:
#' We hard code `http_version = 2L` to use HTTP/1.1 in HTTP requests to
#' the Entrez API. See `curl::curl_symbols('CURL_HTTP_VERSION')` 
#'
#' @author Scott Chamberlain
#' @examples \dontrun{
#' comm2sci(com='american black bear')
#' comm2sci(com='american black bear', simplify = FALSE)
#' comm2sci(com='black bear', db='itis')
#' comm2sci(com='american black bear', db='itis')
#' comm2sci(com='annual blue grass', db='tropicos')
#' comm2sci(com=c('annual blue grass','tree of heaven'), db='tropicos')
#' comm2sci('blue whale', db = "worms")
#' comm2sci(c('blue whale', 'dwarf surfclam'), db = "worms")
#'
#' # ncbi: pass in uid's from get_ncbi() directly
#' x <- get_ncbi("western capercaillie", modifier = "Common Name")
#' comm2sci(x)
#' # itis: pass in tsn's from get_itis() directly
#' x <- get_itis(c("Louisiana black bear", "american crow"),
#'   searchtype = "common")
#' comm2sci(x)
#' }
comm2sci <- function(...) {
  UseMethod("comm2sci")
}

#' @method comm2sci default
#' @export
#' @rdname comm2sci
comm2sci.default <- function(com, db='ncbi', itisby='search',
                             simplify=TRUE, commnames = NULL, ...) {
  pchk(commnames, "com")
  if (!is.null(commnames)) com <- commnames
  assert(com, "character")
  assert(simplify, "logical")
  temp <- lapply(com, sci_from_comm, db = db, simplify = simplify,
                 itisby = itisby, ...)
  stats::setNames(temp, com)
}

sci_from_comm <- function(nn, db, simplify, itisby, ...) {
  switch(
    db,
    eol = c2s_eol(sci = nn, simplify, ...),
    itis = c2s_itis(nn, itisby, simplify, ...),
    tropicos = c2s_tp(simplify, com = nn, ...),
    ncbi = {
      ids <- get_ncbi(nn, modifier = "Common Name", ...)
      c2s_ncbi(ids, ...)
    },
    worms = c2s_worms(nn, simplify, ...),
    stop("'db' must be one of 'ncbi', 'itis', 'tropicos', 'eol', 'worms'",
         call. = FALSE)
  )
}

#' @export
#' @rdname comm2sci
comm2sci.txid <- function(id, db='ncbi', itisby='search',
                         simplify=TRUE, ...) {
  fun <- parse(text=paste0("comm2sci_", id_class(id)))
  db <- as.character(taxa::tax_db(id))[1]
  eval(fun)(id, db, itisby, simplify, ...)
}

comm2sci_itis <- function(id, db='itis', itisby='search',
                         simplify=TRUE, ...) {
  warn_db(list(db = db), "itis")
  temp <- lapply(as.character(taxa::tax_id(id)), c2s_itis_,
    simplify = simplify, ...)
  stats::setNames(temp, as.character(taxa::tax_id(id)))
}

comm2sci_ncbi <- function(id, db='ncbi', itisby='search',
                         simplify=TRUE, ...) {
  warn_db(list(db = db), "ncbi")
  temp <- lapply(id, c2s_ncbi, simplify = simplify, ...)
  stats::setNames(temp, as.character(taxa::tax_id(id)))
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
  } else {
    tmp
  }
}

c2s_ncbi <- function(x, simplify, ...) {
  key <- getkey(NULL, "ENTREZ_KEY")
  query <- tc(list(db = "taxonomy", ID = as.character(taxa::tax_id(x)),
    api_key = key))
  cli <- crul::HttpClient$new(url = ncbi_base(),
    headers = tx_ual, opts = list(http_version = 2L, ...))
  res <- cli$get("entrez/eutils/efetch.fcgi", query = query)
  if (!res$success()) return(character())
  tt <- res$parse("UTF-8")
  ttp <- xml2::read_xml(tt)
  # common name
  out <- xml_text(xml_find_all(ttp, "//TaxaSet/Taxon/ScientificName"))
  # NCBI limits requests to three per second
  ncbi_rate_limit_pause(key)
  return(out)
}

c2s_eol <- function(simplify, ...){
  tmp <- eol_search(...)
  if (all(is.na(tmp))) return(character(0))
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
