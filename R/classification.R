#' Get taxonomic hierarchy for a given taxon.
#' @import XML RCurl
#' @param x quoted tsn number (taxonomic serial number)
#' @return Classification of the taxon in a data.frame.
#' @export
#' @examples \dontrun{
#' classification(685566)
#' }
classification <- 

function (x) 
{
  doc <- get_itis_xml(searchterm = x, searchtype = "tsnfullhir", by_ = "tsn")
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(doc, "//ax23:rankName", namespaces=namespaces)
  rank <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(doc, "//ax23:taxonName", namespaces=namespaces)
  taxon <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(doc, "//ax23:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue) # last one is a repeat
  data.frame(rank, taxon, tsn=tsn[-length(tsn)])
}