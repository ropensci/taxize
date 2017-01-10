#' Get scientific names from common names.
#'
#' @param commnames One or more common names or partial names.
#' @param db Data source, one of \emph{"eol"} (default), \emph{"itis"},
#' \emph{"tropicos"}, \emph{"ncbi"}, or \emph{"worms"}. Note that each
#' taxonomic data source
#' has their own identifiers, so that if you provide the wrong \code{db}
#' value for the identifier you could get a result, but it will likely be
#' wrong (not what you were expecting).
#' @param itisby Search for common names across entire names (search, default),
#' at beginning of names (begin), or at end of names (end).
#' @param simplify (logical) If TRUE, simplify output to a vector of names.
#' If \code{FALSE}, return variable formats from different sources,
#' usually a data.frame.
#' @param ... Further arguments passed on to internal methods.
#' @return A vector of names.
#' @seealso \code{\link[ritis]{search_common}},
#' \code{\link[taxize]{eol_search}},
#' \code{\link[taxize]{tp_search}}
#' @export
#' @seealso \code{\link[taxize]{sci2comm}}
#' @author Scott Chamberlain (myrmecocystus@@gmail.com)
#' @examples \dontrun{
#' comm2sci(commnames='black bear')
#' comm2sci(commnames='black bear', db='itis')
#' comm2sci(commnames='annual blue grass', db='tropicos')
#' comm2sci(commnames=c('annual blue grass','tree of heaven'), db='tropicos')
#' comm2sci(commnames=c('black bear', 'roe deer'))
#' comm2sci('blue whale', db = "worms")
#' comm2sci(c('blue whale', 'dwarf surfclam'), db = "worms")
#'
#' # Output easily converts to a data.frame with \code{\link[plyr]{ldply}}
#' library(plyr)
#' ldply(comm2sci(commnames=c('annual blue grass','tree of heaven'),
#'   db='tropicos'))
#'
#' # Use curl options
#' library("httr")
#' comm2sci(commnames='black bear', config=verbose())
#' comm2sci(commnames='black bear', db="itis", config=verbose())
#' comm2sci(commnames='bear', db="ncbi", config=verbose())
#' comm2sci(commnames='annual blue grass', db="tropicos", config=verbose())
#' }

comm2sci <- function(commnames, db='eol', itisby='search', simplify=TRUE, ...) {
  # ITIS helper
  foo <- function(x, by='search', simplify, ...){
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

  ncbi2sci <- function(x, simplify, ...){
    uid <- get_uid(x, ...)
    if (is.na(uid))
      return(NA)
    baseurl <- paste0(ncbi_base(), "/entrez/eutils/efetch.fcgi?db=taxonomy")
    ID <- paste("ID=", uid, sep = "")
    searchurl <- paste(baseurl, ID, sep = "&")
    tt <- GET(searchurl)
    stop_for_status(tt)
    res <- con_utf8(tt)
    ttp <- xml2::read_xml(res)
    # common name
    out <- xml_text(xml_find_all(ttp, "//TaxaSet/Taxon/ScientificName"))
    # NCBI limits requests to three per second
    Sys.sleep(0.33)
    return(out)
  }

  eol_search_ <- function(simplify, ...){
    tmp <- eol_search(...)
    if (simplify) {
      as.character(tmp$name)
    } else {
      tmp
    }
  }

  tp_search_ <- function(simplify, ...){
    tmp <- tp_search(...)
    if (simplify) {
      as.character(tmp$scientificname)
    } else{
      tmp
    }
  }

  worms_s <- function(x, simplify, ...){
    tmp <- try_df(worrms::wm_records_common(name = x, ...))
    if (simplify) {
      as.character(tmp$scientificname)
    } else{
      tmp
    }
  }

  getsci <- function(nn, simplify, ...) {
    switch(
      db,
      eol = eol_search_(terms = nn, simplify, ...),
      itis = foo(nn, itisby, simplify, ...),
      tropicos = tp_search_(simplify, commonname = nn, ...),
      ncbi = ncbi2sci(nn, ...),
      worms = worms_s(nn, simplify, ...)
    )
  }
  temp <- lapply(commnames, getsci, simplify = simplify, ...)
  names(temp) <- commnames
  temp
}
