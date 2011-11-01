# getacceptname.R

getacceptname <- function(searchtsn = NA, supmess = TRUE,
  url = "http://www.itis.gov/ITISWebService/services/ITISService/getAcceptedNamesFromTSN?tsn=",
  curl = getCurlHandle()){
# Retrieve accepted TSN (with accepted name)
# Args: 
#     searchtsn: quoted TSN for a taxonomic group (character)
# Output: names or TSNs of all downstream taxa
# Note: you can suppress the message output with suppressMessages()
# Examples:
#   getacceptname('208527')  # TSN accepted - good name
#   getacceptname('504239')  # TSN not accepted - input TSN is old name
    
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