#' Retrieve all taxa names or TSNs downstream in hierarchy from given TSN.
#' @import XML RCurl
#' @param searchtsn Quoted TSN for a taxonomic group (character).
#' @param attachrank Should rank be attached to the output? (default=TRUE).
#' @param url The ITIS url for the function (should be left to default).
#' @param curl If using in a loop, call getCurlHandle() first and pass 
#'  the returned value in here (avoids unnecessary footprint)
#' @return Names or TSNs of all downstream taxa in a data.frame.
#' @export
#' @examples \dontrun{
#' downstream(searchtsn = '208527')
#' }
downstream <- 

function(searchtsn = NA, attachrank = TRUE,
  url = "http://www.itis.gov/ITISWebService/services/ITISService/getHierarchyDownFromTSN?tsn=",
  curl = getCurlHandle())
{
    newurl <- paste(url, searchtsn, sep = '')
    tt <- getURLContent(newurl, curl=curl)  
    tt_ <- xmlParse(tt)
    namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
    nodes <- getNodeSet(tt_, "//ax23:taxonName/..", namespaces=namespaces)
    sci <- sapply(nodes, function(x) xmlValue(x[["taxonName"]]))
    tsn <- sapply(nodes, function(x) xmlValue(x[["tsn"]]))
    dat <- data.frame(sci, tsn)
    
    if(!attachrank == TRUE) { dat } else
      { ranks <- sapply(temp[,2], gettaxrank)
        data.frame(temp, ranks) }
}