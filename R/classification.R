# classification.R

classification <- function (x) {
# Function to get taxonomy information. <cboettig>
#   input: x = quoted tsn number (taxonomic serial number)
# Example:
#   classification(685566)
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