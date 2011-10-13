# gettaxrank.R

gettaxrank <- function(searchtsn = NA,
  url = "http://www.itis.gov/ITISWebService/services/ITISService/getTaxonomicRankNameFromTSN?tsn=",
  curl = getCurlHandle()){
# Retrieve taxanomic rank name from given TSN
# Args: 
#     searchtsn: quoted TSN for a taxonomic group (character)
# Output: taxonomic rank name
# Examples:
#   gettaxrank('202385')
    newurl <- paste(url, searchtsn, sep = '')
    tt <- getURLContent(newurl, curl=curl)  
    tt_ <- xmlParse(tt)
    xmlToList(tt_)$return$rankName
}