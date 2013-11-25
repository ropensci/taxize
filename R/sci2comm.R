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
  itis2comm <- function(x, ...){
    # get tsn
    tsn <- get_tsn(x, ...)
    # if tsn is not found
    if(is.na(tsn)) {
      out <- NA
    } else {
      out <- as.character(getcommonnamesfromtsn(tsn)$comname)
      #if common name is not found
      if(length(out) == 0)
        out <- NA
      }
    # name list
    return(out)
  }

  eol2comm <- function(x){
    tmp <- eol_search(terms=x)
    pageids <- tmp[grep(x, tmp$name), "pageid"]
    dfs <- compact(lapply(pageids, function(x) eol_pages(taxonconceptID=x, common_names=TRUE)$vernac))
    ldply(dfs[sapply(dfs, class)=="data.frame"])
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
           eol = eol2comm(x = nn),
           itis = itis2comm(nn, ...),
           ncbi = ncbi2comm(nn, ...))
  }
  temp <- lapply(scinames, function(x) getsci(x, ...))
  names(temp) <- scinames
  temp
}





# #' @export
# #' @keywords internal
# tropicoscommnamesearch <- function(x, ...){
#   tmp <- tp_search(x, ...)
#   df <- tmp[,c('NameId','ScientificName','RankAbbreviation','NomenclatureStatusName')]
#   names(df) <- c('tpsid','name','rank','status')
#   df
# }