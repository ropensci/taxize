#' Get scientific names from common names.
#'
#' @param commnames One or more common names or partial names.
#' @param db Data source, one of \emph{"eol"} (default), \emph{"itis"}, \emph{"tropicos"}
#'    or \emph{"ncbi"}.
#' @param itisby Search for common names across entire names (search, default),
#'    at beginning of names (begin), or at end of names (end).
#' @param simplify (logical) If TRUE, simplify output to a vector of names. If FALSE,
#'    return variable formats from different sources, usually a data.frame.
#' @param ... Further arguments passed on to internal methods.
#' @return A vector of names.
#' @seealso \code{\link[taxize]{searchbycommonname}},
#' \code{\link[taxize]{searchbycommonnamebeginswith}},
#' \code{\link[taxize]{searchbycommonnameendswith}}, \code{\link[taxize]{eol_search}},
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
#'
#' # Output easily converts to a data.frame with \code{\link[plyr]{ldply}}
#' library(plyr)
#' ldply(comm2sci(commnames=c('annual blue grass','tree of heaven'), db='tropicos'))
#' }

comm2sci <- function(commnames, db='eol', itisby='search', simplify=TRUE, ...)
{
  foo <- function(x, by='search', simplify, ...){
    tmp <- switch(by,
                  search = searchbycommonname(x, ...),
                  begin = searchbycommonnamebeginswith(x, ...),
                  end = searchbycommonnameendswith(x, ...))
    # remove empty tsn slots
    tsns <- as.character(tmp$tsn)
    tsns <- tsns[!sapply(tsns, nchar)==0]
    # get scientific names
    tmp <- do.call(rbind, lapply(tsns, getscientificnamefromtsn))
    if(simplify){
      as.character(tmp$combinedName)
    } else{ tmp }
  }

  ncbi2sci <- function(x, simplify, ...){
    uid <- get_uid(x, ...)
    if(is.na(uid))
      return(NA)
    baseurl <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=taxonomy"
    ID <- paste("ID=", uid, sep = "")
    searchurl <- paste(baseurl, ID, sep = "&")
    tt <- getURL(searchurl)
    ttp <- xmlTreeParse(tt, useInternalNodes = TRUE)
    # common name
    out <- xpathSApply(ttp, "//TaxaSet/Taxon/ScientificName", xmlValue)
    # NCBI limits requests to three per second
    Sys.sleep(0.33)
    return(out)
  }

  eol_search_ <- function(simplify, ...){
    tmp <- eol_search(...)
    if(simplify){
      as.character(tmp$name)
    } else{ tmp }
  }

  tp_search_ <- function(simplify, ...){
    tmp <- tp_search(...)
    if(simplify){
      as.character(tmp$scientificname)
    } else{ tmp }
  }

  getsci <- function(nn, ...){
    switch(db,
           eol = eol_search_(terms=nn, simplify, ...),
           itis = foo(nn, itisby, simplify, ...),
           tropicos = tp_search_(simplify, commonname = nn, ...),
           ncbi = ncbi2sci(nn))
  }
  temp <- lapply(commnames, function(x) getsci(x, ...))
  names(temp) <- commnames
  temp
}
