# downstream.R

downstream <- function(searchtsn = NA, attachrank = TRUE,
  url = "http://www.itis.gov/ITISWebService/services/ITISService/getHierarchyDownFromTSN?tsn=",
  curl = getCurlHandle()){
# Retrieve all taxa names or TSNs downstream in hierarchy from given TSN
# Args: 
#     searchtsn: quoted TSN for a taxonomic group (character)
# Output: names or TSNs of all downstream taxa
# Examples:
#   downstream(searchtsn = '208527')

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