#' Get common names from scientific names.
#' 
#' @param scinames One or more scientific names or partial names.
#' @param db Data source, one of eol (default) or itis
#' @param ... Further arguments passed on to functions
#' @return List of data.frame's.
#' @seealso \code{\link[taxize]{searchbycommonname}}, 
#' \code{\link[taxize]{searchbycommonnamebeginswith}}, 
#' \code{\link[taxize]{searchbycommonnameendswith}}, \code{\link[taxize]{eol_search}},
#' \code{\link[taxize]{tp_search}}
#' @export
#' @seealso \code{\link[taxize]{comm2sci}}
#' @author Scott Chamberlain (myrmecocystus@@gmail.com)
#' @examples \dontrun{
#' sci2comm(scinames='Helianthus annuus')
#' sci2comm(scinames='Helianthus annuus', db='itis')
#' sci2comm(scinames=c('black bear', 'roe deer'))
#' }
sci2comm <- function(scinames, db='eol', ...)
{  
  getsci <- function(nn, ...){
    switch(db, 
           eol = eolsearch2(x=nn),
           itis = itiscommnamesearch(nn, ...))
  }
  temp <- lapply(scinames, function(x) getsci(x, ...))
  names(temp) <- scinames
  temp
}

#' @export
#' @keywords internal
eolsearch2 <- function(x){
  tmp <- eol_search(terms=x)
  pageids <- tmp[grep(x, tmp$name), "pageid"]
  dfs <- compact(lapply(pageids, function(x) eol_pages(taxonconceptID=x, common_names=TRUE)$vernac))
  ldply(dfs[sapply(dfs, class)=="data.frame"])
}

#' @export
#' @keywords internal
itiscommnamesearch <- function(x, ...){
  out <- searchbyscientificname(x, ...)
  # remove empty tsn slots
  tsns <- as.character(out$tsn)
  tsns <- tsns[!sapply(tsns, nchar)==0]
  # get scientific names
  temp <- lapply(tsns, getcommonnamesfromtsn)
  temp <- temp[!sapply(temp, nrow)==0]
  do.call(rbind, temp)
}

# #' @export
# #' @keywords internal
# tropicoscommnamesearch <- function(x, ...){
#   tmp <- tp_search(x, ...)
#   df <- tmp[,c('NameId','ScientificName','RankAbbreviation','NomenclatureStatusName')]
#   names(df) <- c('tpsid','name','rank','status')
#   df
# }