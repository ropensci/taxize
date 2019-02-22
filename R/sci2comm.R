#' Get common names from scientific names.
#'
#' @export
#' @param scinames character; One or more scientific names or partial names.
#' @param db character; Data source, one of \emph{"ncbi"} (default),
#' \emph{"itis"} \emph{"eol"}, \emph{"worms"}, or \emph{"iucn"}. Note that
#' each taxonomic data source has their own identifiers,  so that if you
#' provide the wrong \code{db} value for the identifier you could get a
#' result, but it will likely be wrong (not what you were expecting). 
#' If using ncbi, eol or iucn we recommend getting an API key; 
#' see \code{\link{taxize-authentication}}
#' @param simplify (logical) If TRUE, simplify output to a vector of names.
#' If FALSE, return variable formats from different sources, usually a
#' data.frame. Only applies to eol and itis. Specify \code{FALSE} to obtain
#' the language of each vernacular in the output for eol and itis.
#' @param ... Further arguments passed on to functions
#' \code{\link[taxize]{get_uid}}, \code{\link[taxize]{get_tsn}}.
#' @param id character; identifiers, as returned by
#' \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{get_uid}}.
#'
#' @section Authentication:
#' See \code{\link{taxize-authentication}} for help on authentication
#' 
#' @return List of character vectors, named by input taxon name, or taxon ID
#'
#' @seealso \code{\link[taxize]{comm2sci}}
#'
#' @author Scott Chamberlain (myrmecocystus@@gmail.com)
#'
#' @examples \dontrun{
#' sci2comm(scinames='Helianthus annuus')
#' sci2comm(scinames='Helianthus annuus', db='eol')
#' sci2comm(scinames='Helianthus annuus', db='itis')
#' sci2comm(scinames=c('Helianthus annuus', 'Poa annua'))
#' sci2comm(scinames='Puma concolor', db='ncbi')
#' sci2comm('Gadus morhua', db='worms')
#' sci2comm('Pomatomus saltatrix', db='worms')
#' sci2comm('Loxodonta africana', db='iucn')
#'
#' # Passing id in, works for sources: itis and ncbi, not eol
#' sci2comm(get_tsn('Helianthus annuus'))
#' sci2comm(get_uid('Helianthus annuus'))
#' sci2comm(get_wormsid('Gadus morhua'))
#' sci2comm(get_iucn('Loxodonta africana'))
#'
#' # Don't simplify returned
#' sci2comm(get_tsn('Helianthus annuus'), simplify=FALSE)
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
sci2comm.default <- function(scinames, db='ncbi', simplify=TRUE, ...) {
  temp <- lapply(scinames, getsci, db = db, simplify = simplify, ...)
  stats::setNames(temp, scinames)
}

#' @export
#' @rdname sci2comm
sci2comm.uid <- function(id, ...) {
  out <- lapply(id, function(x) ncbi_foo(x, ...))
  names(out) <- id
  return(out)
}

#' @export
#' @rdname sci2comm
sci2comm.tsn <- function(id, simplify=TRUE, ...) {
  out <- lapply(id, function(x) itis_foo(x, simplify, ...))
  names(out) <- id
  return(out)
}

#' @export
#' @rdname sci2comm
sci2comm.wormsid <- function(id, simplify=TRUE, ...) {
  out <- lapply(id, function(x) worms_foo(x, simplify, ...))
  names(out) <- id
  return(out)
}

#' @export
#' @rdname sci2comm
sci2comm.iucn <- function(id, simplify=TRUE, ...) {
  #out <- lapply(id, function(x) iucn_foo(x, simplify, ...))
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
  tmp <- eol_search(terms = x, ...)
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
  tt <- ldply(dfs[sapply(dfs, class) == "data.frame"])
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
    out <- NA
  } else {
    out <- ritis::common_names(x)
    #if common name is not found
    if (nrow(out) == 0) {
      out <- NA
    }
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
    opts = list(...))
  res <- cli$get("entrez/eutils/efetch.fcgi", query = query)
  res$raise_for_status()
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
    out <- NA
  } else {
    out <- worrms::wm_common_id(as.numeric(x))
    #if common name is not found
    if (nrow(out) == 0) {
      out <- NA
    }
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
    out <- NA
  } else {
    out <- rredlist::rl_common_names(name = x, ...)
    # if common name is not found
    if (NROW(out$result) == 0) {
      out <- NA
    }
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
