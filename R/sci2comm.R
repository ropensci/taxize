#' Get common names from scientific names.
#' 
#' @param scinames character; One or more scientific names or partial names.
#' @param db character; Data source, one of \emph{"eol"} (default), \emph{"itis"} 
#' or \emph{"ncbi"}.
#' @param simplify (logical) If TRUE, simplify output to a vector of names. If FALSE,
#'    return variable formats from different sources, usually a data.frame.
#' @param ... Further arguments passed on to functions \code{\link[taxize]{get_uid}} or 
#' \code{\link[taxize]{get_tsn}}.
#' @param id character; identifiers, as returned by \code{\link[taxize]{get_tsn}} or
#'    \code{\link[taxize]{get_uid}}.
#' 
#' @return List of character - vectors.
#' 
#' @note \emph{"ncbi"} and uid-method return common names from GenBank.
#' @seealso \code{\link[taxize]{searchbycommonname}}, 
#' \code{\link[taxize]{searchbycommonnamebeginswith}}, 
#' \code{\link[taxize]{searchbycommonnameendswith}}, \code{\link[taxize]{eol_search}},
#' \code{\link[taxize]{tp_search}}, \code{\link[taxize]{comm2sci}}
#' @export
#' @author Scott Chamberlain (myrmecocystus@@gmail.com)
#' @examples \dontrun{
#' sci2comm(scinames='Helianthus annuus')
#' sci2comm(scinames='Helianthus annuus', db='itis')
#' sci2comm(scinames=c('Helianthus annuus', 'Poa annua'))
#' sci2comm(scinames='Puma concolor', db='ncbi')
#' 
#' # Passing id in, works for sources: itis and ncbi
#' sci2comm(get_tsn('Helianthus annuus'))
#' sci2comm(get_uid('Helianthus annuus'))
#' 
#' # Don't simplify returned
#' sci2comm(get_tsn('Helianthus annuus'), simplify=FALSE)
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
  itis2comm <- function(x, simplify, ...){
    # get tsn
    tsn <- get_tsn(x, ...)
    # if tsn is not found
    if(is.na(tsn)) {
      out <- NA
    } else {
      out <- getcommonnamesfromtsn(tsn)
      #if common name is not found
      if(nrow(out) == 0)
        out <- NA
      }
    if(simplify){
      as.character(out$comname)
    } else{ out }
  }

  eol2comm <- function(x, simplify){
    tmp <- eol_search(terms=x)
    pageids <- tmp[grep(x, tmp$name), "pageid"]
    dfs <- compact(lapply(pageids, function(x) eol_pages(taxonconceptID=x, common_names=TRUE)$vernac))
    tt <- ldply(dfs[sapply(dfs, class)=="data.frame"])
    if(simplify){
      ss <- as.character(tt$vernacularname)
      ss[ !is.na(ss) ]
    } else{ tt }
  }
  
  ncbi2comm <- function(x, ...){
    uid <- get_uid(x, ...)
    
    baseurl <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=taxonomy"
    ID <- paste("ID=", uid, sep = "")
    searchurl <- paste(baseurl, ID, sep = "&")
    tt <- getURL(searchurl)
    ttp <- xmlTreeParse(tt, useInternalNodes = TRUE)
    # common name
    out <- xpathSApply(ttp, "//TaxaSet/Taxon/OtherNames/GenbankCommonName", xmlValue)
    # NCBI limits requests to three per second
    Sys.sleep(0.33)
    return(out)
  }

  getsci <- function(nn, ...){
    switch(db, 
           eol = eol2comm(x = nn, simplify),
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
  ncbi2comm <- function(uid, ...){
    baseurl <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=taxonomy"
    ID <- paste("ID=", uid, sep = "")
    searchurl <- paste(baseurl, ID, sep = "&")
    tt <- getURL(searchurl)
    ttp <- xmlTreeParse(tt, useInternalNodes = TRUE)
    # common name
    out <- xpathSApply(ttp, "//TaxaSet/Taxon/OtherNames/GenbankCommonName", xmlValue)
    # NCBI limits requests to three per second
    Sys.sleep(0.33)
    return(out)
  }
  out <- lapply(id, function(x) ncbi2comm(x))
  names(out) <- id
  return(out)
}

#' @method sci2comm tsn
#' @export
#' @rdname sci2comm
sci2comm.tsn <- function(id, simplify=TRUE, ...){
  itis2comm <- function(id, ...){
    # if tsn is not found
    if(is.na(id)) {
      out <- NA
    } else {
      out <- getcommonnamesfromtsn(id)
      #if common name is not found
      if(length(out) == 0)
        out <- NA
    }
    if(simplify){
      as.character(out$comname)
    } else{ out }
  }
  out <- lapply(id, function(x) itis2comm(x))
  names(out) <- id
  return(out)
}