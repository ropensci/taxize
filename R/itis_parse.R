#' Return the scientific name, a common name, and tsn of all matches to an itis query.
#' 
#' @import XML
#' @param doc A query result from ITIS in xml format.
#' @return Taxonomic rank name.
#' @export
#' @examples \dontrun{
#' itis_parse()
#' }
itis_parse  <- function()
{
#   namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
#   nodes <- getNodeSet(doc, "//ax23:sciName/..", namespaces=namespaces)
#   sci <- sapply(nodes, function(x) xmlValue(x[["sciName"]]))
#   tsn <- sapply(nodes, function(x) xmlValue(x[["tsn"]]))
#   com <- sapply(nodes, function(x) xmlValue(x[["commonNameList"]][[1]][[1]]))
#   data.frame(sci, com, tsn)
	message("itis_parse deprecated")
}