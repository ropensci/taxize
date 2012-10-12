#' Get family names to make Phylomatic input object.
#' 
#' @import XML RCurl plyr
#' @param x Quoted tsn number (taxonomic serial number).
#' @return Family name for the searched taxon.
#' @export
#' @examples \dontrun{
#' itis_get_familyname()
#' }
itis_get_familyname <- function (x) 
{
  temp <- get_itis_xml(searchterm = x, searchtype = "tsnfullhir", by_ = "tsn")
  tempdf <- ldply(xmlToList(temp)$return, function(x) data.frame(c(x[3], x[4])))
  as.character(tempdf[tempdf$rankName == 'Family', 3])[1]
}