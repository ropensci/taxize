#' Retrieve accepted TSN (with accepted name).
#' 
#' @import XML RCurl
#' @param searchtsn Quoted TSN for a taxonomic group (character).
#' @param supmess Suppress informative messages (default=TRUE).
#' @param url The ITIS API url for the function (should be left to default).
#' @param curl If using in a loop, call getCurlHandle() first and pass 
#'    the returned value in here (avoids unnecessary footprint)
#' @details You can print informative messages by setting supmess=FALSE.
#' @return Names or TSNs of all downstream taxa.
#' @export
#' @examples \dontrun{
#' itis_getacceptname('208527')  # TSN accepted - good name
#' itis_getacceptname('504239')  # TSN not accepted - input TSN is old name
#' }
itis_getacceptname <- function(searchtsn = NA, supmess = TRUE,
  url = "http://www.itis.gov/ITISWebService/services/ITISService/getAcceptedNamesFromTSN?tsn=",
  curl = getCurlHandle())
{    
    newurl <- paste(url, searchtsn, sep = '')
    tt <- getURLContent(newurl, curl=curl)  
    tt_ <- xmlParse(tt)
    temp <- xmlToList(tt_)
    if(supmess == FALSE) {
      if(length(temp$return$acceptedNames) == 1) 
        { message("Good name!")
          temp$return$tsn 
          } else
        { message("Bad name!")
          temp$return$acceptedNames[1:2] 
          }
    } else
        { if(length(temp$return$acceptedNames) == 1) { temp$return$tsn } else
            { temp$return$acceptedNames[1:2] } }
}