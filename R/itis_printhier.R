#' Print taxnomic hierarchy.
#' 
#' @import XML plyr
#' @param hierout Output from a hierarchy search using get_itis_xml function.
#' @return Taxonomic hierarchy as data.fram.
#' @export
#' @examples \dontrun{
#' out <- get_itis_xml("36616", "tsnfullhir", "tsn", FALSE)  
#' itis_printhier(out)  
#' }
itis_printhier <- function(hierout = NA) 
{
#   page <- xmlTreeParse(hierout)
#   templist <- ldply(xmlToList(page), function(x) data.frame(c(x[3], x[4])))[,-3]
#   na.omit(templist)
	message("itis_printhier deprecated")
}