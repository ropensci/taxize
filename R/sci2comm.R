#' Get common names from scientific names.
#'
#' @export
#'
#' @param scinames character; One or more scientific names or partial names.
#' @param db character; Data source, one of \emph{"eol"} (default), \emph{"itis"}
#'   or \emph{"ncbi"}.
#' @param simplify (logical) If TRUE, simplify output to a vector of names. If FALSE,
#'    return variable formats from different sources, usually a data.frame. Only applies to
#'    eol and itis.
#' @param ... Further arguments passed on to functions \code{\link[taxize]{get_uid}},
#'    \code{\link[taxize]{get_tsn}}.
#' @param id character; identifiers, as returned by \code{\link[taxize]{get_tsn}},
#'    \code{\link[taxize]{get_uid}}.
#'
#' @details Note that EOL requires an API key. You can pass in your EOL api key in the function
#' call like \code{sci2comm('Helianthus annuus', key="<your eol api key>")}. You can also store your
#' EOL API key in your .Rprofile file as \code{options(eolApiKey = "<your eol api key>")}, or
#' just for the current session by running \code{options(eolApiKey = "<your eol api key>")} in
#' the console.
#'
#' @return List of character vectors.
#'
#' @seealso \code{\link[taxize]{searchbycommonname}},
#' \code{\link[taxize]{searchbycommonnamebeginswith}},
#' \code{\link[taxize]{searchbycommonnameendswith}}, \code{\link[taxize]{eol_search}},
#' \code{\link[taxize]{tp_search}}, \code{\link[taxize]{comm2sci}}
#'
#' @author Scott Chamberlain (myrmecocystus@@gmail.com)
#'
#' @examples \dontrun{
#' sci2comm(scinames='Helianthus annuus', db='eol')
#' sci2comm(scinames='Helianthus annuus', db='itis')
#' sci2comm(scinames=c('Helianthus annuus', 'Poa annua'))
#' sci2comm(scinames='Puma concolor', db='ncbi')
#'
#' # Passing id in, works for sources: itis and ncbi, not eol
#' sci2comm(get_tsn('Helianthus annuus'))
#' sci2comm(get_uid('Helianthus annuus'))
#'
#' # Don't simplify returned
#' sci2comm(get_tsn('Helianthus annuus'), simplify=FALSE)
#'
#' # Use curl options
#' library("httr")
#' sci2comm(scinames='Helianthus annuus', config=verbose())
#' sci2comm('Helianthus annuus', db="itis", config=verbose())
#' sci2comm('Helianthus annuus', db="ncbi", config=verbose())
#' }
#' @rdname sci2comm
sci2comm <- function(...){
  UseMethod("sci2comm")
}

#' @method sci2comm default
#' @export
#' @rdname sci2comm
sci2comm.default <- function(scinames, db='eol', simplify=TRUE, ...)
{
  itis2comm <- function(x, simplify=TRUE, ...){
    tsn <- get_tsn(x, ...)
    itis_foo(tsn, simplify, ...)
  }

  eol2comm <- function(x, ...){
    tmp <- eol_search(terms=x, ...)
    pageids <- tmp[grep(x, tmp$name), "pageid"]
    dfs <- tc(lapply(pageids, function(x) eol_pages(taxonconceptID=x, common_names=TRUE, ...)$vernac))
    tt <- ldply(dfs[sapply(dfs, class)=="data.frame"])
    if(simplify){
      ss <- as.character(tt$vernacularname)
      ss[ !is.na(ss) ]
    } else{ tt }
  }

  ncbi2comm <- function(x, ...){
    uid <- get_uid(x, ...)
    ncbi_foo(uid, ...)
  }

  getsci <- function(nn, ...){
    switch(db,
           eol = eol2comm(nn, simplify, ...),
           itis = itis2comm(nn, simplify, ...),
           ncbi = ncbi2comm(nn, ...))
  }
  temp <- lapply(scinames, function(x) getsci(x, ...))
  names(temp) <- scinames
  temp
}

#' @method sci2comm uid
#' @export
#' @rdname sci2comm
sci2comm.uid <- function(id, ...)
{
  out <- lapply(id, function(x) ncbi_foo(x, ...))
  names(out) <- id
  return(out)
}

#' @method sci2comm tsn
#' @export
#' @rdname sci2comm
sci2comm.tsn <- function(id, simplify=TRUE, ...){
  out <- lapply(id, function(x) itis_foo(x, ...))
  names(out) <- id
  return(out)
}

itis_foo <- function(x, simplify=TRUE, ...){
  # if tsn is not found
  if(is.na(x)) {
    out <- NA
  } else {
    out <- getcommonnamesfromtsn(x)
    #if common name is not found
    if(nrow(out) == 0)
      out <- NA
  }
  if(simplify){
    if(!is(out, "data.frame")) out else as.character(out$comname)
  } else{ out }
}

ncbi_foo <- function(x, ...){
  baseurl <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=taxonomy"
  ID <- paste("ID=", x, sep = "")
  searchurl <- paste(baseurl, ID, sep = "&")
  tt <- getURL(searchurl)
  ttp <- xmlTreeParse(tt, useInternalNodes = TRUE)
  # common name
  out <- xpathSApply(ttp, "//TaxaSet/Taxon/OtherNames/GenbankCommonName", xmlValue)
  # NCBI limits requests to three per second
  Sys.sleep(0.33)
  return(out)
}
