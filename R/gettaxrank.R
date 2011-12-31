#' Retrieve taxanomic rank name from given TSN.
#' @import XML RCurl
#' @param searchtsn Quoted TSN for a taxonomic group (character).
#' @param url The ITIS API url for the function (should be left to default).
#' @param curl If using in a loop, call getCurlHandle() first and pass 
#'    the returned value in here (avoids unnecessary footprint)
#' @details You can print informative messages by setting supmess=FALSE.
#' @return Taxonomic rank name.
#' @export
#' @examples \dontrun{
#' gettaxrank('202385')
#' }
gettaxrank <- 
  
function(searchtsn = NA,
  url = "http://www.itis.gov/ITISWebService/services/ITISService/getTaxonomicRankNameFromTSN?tsn=",
  curl = getCurlHandle())
{
    newurl <- paste(url, searchtsn, sep = '')
    tt <- getURLContent(newurl, curl=curl)  
    tt_ <- xmlParse(tt)
    xmlToList(tt_)$return$rankName
}