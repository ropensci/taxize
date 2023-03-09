#' Get common names from scientific names.
#'
#' @export
#' @param sci character; One or more scientific names or partial names.
#' @param db character; Data source, one of `"ncbi"` (default),
#' `"itis"` `"eol"`, `"worms"`, or `"iucn"`. Note that
#' each taxonomic data source has their own identifiers,  so that if you
#' provide the wrong `db` value for the identifier you could get a
#' result, but it will likely be wrong (not what you were expecting). 
#' If using ncbi or iucn we recommend getting an API key; 
#' see [taxize-authentication]
#' @param simplify (logical) If TRUE, simplify output to a vector of names.
#' If FALSE, return variable formats from different sources, usually a
#' data.frame. Only applies to eol and itis. Specify `FALSE` to obtain
#' the language of each vernacular in the output for eol and itis.
#' @param scinames Deprecated, see `sci`
#' @param ... Further arguments passed on to functions [get_uid()],
#' [get_tsn()].
#' @param id character; identifiers, as returned by [get_tsn()],
#' [get_uid()].
#'
#' @section Authentication:
#' See [taxize-authentication] for help on authentication
#' 
#' @section HTTP version for NCBI requests:
#' We hard code `http_version = 2L` to use HTTP/1.1 in HTTP requests to
#' the Entrez API. See `curl::curl_symbols('CURL_HTTP_VERSION')` 
#' 
#' @return List of character vectors, named by input taxon name,
#' or taxon ID. `character(0)` on no match
#'
#' @seealso [comm2sci()]
#'
#' @author Scott Chamberlain
#'
#' @examples \dontrun{
#' sci2comm(sci='Helianthus annuus')
#' sci2comm(sci='Helianthus annuus', db='eol')
#' sci2comm(sci=c('Helianthus annuus', 'Poa annua'))
#' sci2comm(sci='Puma concolor', db='ncbi')
#' sci2comm('Gadus morhua', db='worms')
#' sci2comm('Pomatomus saltatrix', db='worms')
#' sci2comm('Loxodonta africana', db='iucn')
#'
#' # Passing id in, works for sources: itis and ncbi, not eol
#' sci2comm(get_uid('Helianthus annuus'))
#' sci2comm(get_wormsid('Gadus morhua'))
#' sci2comm(get_iucn('Loxodonta africana'))
#'
#' # Don't simplify returned
#' sci2comm(get_iucn('Loxodonta africana'), simplify=FALSE)
#'
#' # Use curl options
#' sci2comm('Helianthus annuus', db="ncbi", verbose = TRUE)
#' }
#' @rdname sci2comm
sci2comm <- function(...){
  UseMethod("sci2comm")
}

#' @method sci2comm default
#' @export
#' @rdname sci2comm
sci2comm.default <- function(sci, db='ncbi', simplify=TRUE,
  scinames = NULL, ...) {

  if (!is.null(scinames)) {
    lifecycle::deprecate_warn(when = "v0.9.97", what = "sci2comm(scinames)", with = "sci2comm(sci)")
    sci <- scinames
  }
  
  temp <- lapply(sci, getsci, db = db, simplify = simplify, ...)
  stats::setNames(temp, sci)
}

#' @export
#' @rdname sci2comm
sci2comm.uid <- function(id, ...) {
  warn_db(list(...), "ncbi")
  out <- lapply(id, function(x) ncbi_foo(x, ...))
  names(out) <- id
  return(out)
}

#' @export
#' @rdname sci2comm
sci2comm.tsn <- function(id, simplify=TRUE, ...) {
  warn_db(list(...), "itis")
  out <- lapply(id, function(x) itis_foo(x, simplify, ...))
  names(out) <- id
  return(out)
}

#' @export
#' @rdname sci2comm
sci2comm.wormsid <- function(id, simplify=TRUE, ...) {
  warn_db(list(...), "worms")
  out <- lapply(id, function(x) worms_foo(x, simplify, ...))
  names(out) <- id
  return(out)
}

#' @export
#' @rdname sci2comm
sci2comm.iucn <- function(id, simplify=TRUE, ...) {
  warn_db(list(...), "iucn")
  out <- vector("list", length(id))
  for (i in seq_along(id)) {
    out[[i]] <- iucn_foo(attr(id, "name")[i], simplify, ...)
  }
  names(out) <- id
  return(out)
}



itis2comm <- function(x, simplify, ...){
  tsn <- get_tsn(x, ...)
  itis_foo(tsn, simplify = simplify, ...)
}

eol2comm <- function(x, simplify, ...){
  tmp <- eol_search(x, ...)
  pageids <- tmp[grep(x, tmp$name, ignore.case = TRUE), "pageid"]
  dfs <- tc(
    lapply(pageids, function(x) {
      tmp <- tryCatch(
        eol_pages(taxonconceptID = x, common_names = TRUE, ...),
        error = function(e) e
      )
      if (inherits(tmp, "error")) NULL else tmp$vernacular
    })
  )
  tt <- dt2df(dfs[sapply(dfs, class) == "data.frame"], idcol = FALSE)
  tt <- tt[!duplicated(tt), ]
  if (simplify) {
    ss <- as.character(tt$vernacularname)
    ss[ !is.na(ss) ]
  } else{
    tt
  }
}

ncbi2comm <- function(x, ...){
  uid <- get_uid(x, ...)
  ncbi_foo(uid, ...)
}

worms2comm <- function(x, simplify, ...){
  id <- get_wormsid(x, ...)
  worms_foo(id, simplify = simplify, ...)
}

iucn2comm <- function(x, simplify, ...){
  id <- get_iucn(x, ...)
  iucn_foo(attr(id, "name"), simplify = simplify, ...)
}

getsci <- function(nn, db, simplify, ...){
  switch(
    db,
    eol = eol2comm(nn, simplify, ...),
    itis = itis2comm(nn, simplify, ...),
    ncbi = ncbi2comm(nn, ...),
    worms = worms2comm(nn, simplify, ...),
    iucn = iucn2comm(nn, simplify, ...)
  )
}


itis_foo <- function(x, simplify=TRUE, ...){
  # if tsn is not found
  if (is.na(x)) {
    return(character(0))
  } else {
    out <- ritis::common_names(x)
    # if common name is not found
    if (nrow(out) == 0) return(character(0))
  }
  if (simplify) {
    if (!inherits(out, "tbl_df")) out else as.character(out$commonName)
  } else{
    out
  }
}

ncbi_foo <- function(x, ...){
  key <- getkey(NULL, "ENTREZ_KEY")
  query <- tc(list(db = "taxonomy", ID = x, api_key = key))
  cli <- crul::HttpClient$new(url = ncbi_base(), headers = tx_ual,
    opts = list(http_version = 2L, ...))
  res <- cli$get("entrez/eutils/efetch.fcgi", query = query)
  if (!res$success()) return(character(0))
  tt <- res$parse("UTF-8")
  ttp <- xml2::read_xml(tt)
  # common name
  out <- xml_text(
    xml_find_all(ttp,
      "//TaxaSet/Taxon/OtherNames/GenbankCommonName"))
  # NCBI limits requests to three per second
  if (is.null(key)) Sys.sleep(0.33)
  return(out)
}

worms_foo <- function(x, simplify=TRUE, ...){
  # if id is not found
  if (is.na(x)) {
    return(character(0))
  } else {
    out <- worrms::wm_common_id(as.numeric(x))
    #if common name is not found
    if (nrow(out) == 0) return(character(0))
  }
  if (simplify) {
    if (!inherits(out, "tbl_df")) out else as.character(out$vernacular)
  } else{
    out
  }
}

iucn_foo <- function(x, simplify=TRUE, ...){
  # if id is not found
  if (is.na(x)) {
    return(character(0))
  } else {
    out <- rredlist::rl_common_names(name = x, ...)
    # if common name is not found
    if (NROW(out$result) == 0) return(character(0))
  }
  if (simplify) {
    if (!inherits(out$result, "data.frame")) {
      out$result
    } else {
      as.character(out$result$taxonname)
    }
  } else{
    out$result
  }
}
