#' This function will return all ClassificationBank data pertaining to a 
#' particular ClassificationBankID
#' 
#' @import httr XML RCurl plyr
#' @param namebankID (string) - NameBank identifier you wish to search for in 
#'    ClassificationBank
#' @param classificationTitleID Include if you only which to search within a 
#'    particular classification
#' @param keyCode Your uBio API key; loads from .Rprofile. If you don't have 
#'    one, obtain one at http://www.ubio.org/index.php?pagename=form.
#' @param callopts Parameters passed on to httr::GET call.
#' @return A list of four data.frame's, one for the name itself, one for synonyms, 
#' one for vernacular names, and one for citations.
#' @examples \dontrun{
#' ubio_classification_search(namebankID = 3070378)
#' }
#' @export
ubio_classification_search <- function(namebankID = NULL, classificationTitleID = NULL, 
  keyCode = NULL, callopts=list())
{
  message("this function doesn't work yet, last time checked the API returned no data")
#   url <- "http://www.ubio.org/webservices/service.php"
#   keyCode <- getkey(keyCode, "ubioApiKey")
#   args <- compact(list(
#     'function' = 'classificationbank_search', namebankID = namebankID, 
#     classificationTitleID = classificationTitleID, keyCode = keyCode))
#   tmp <- GET(url, query=args, callopts)
#   stop_for_status(tmp)
#   tt <- content(tmp)  
#   toget <- c("classificationData/classificationTitleID", "classificationData/classificationTitle", 
#              "classificationData/classificationRoot", "rankName", "rankID", "classificationsID", 
#              "recordedName/namebankID", "recordedName/nameString")
#   temp <- lapply(toget, function(x) sapply(xpathApply(tt, paste("/results/", x, sep="")), xmlValue))
#   temp[c(2,8)] <- sapply(temp[c(2,8)], base64Decode)
#   out <- data.frame(do.call(cbind, temp))
#   names(out) <- c("classificationTitleID", "classificationTitle", 
#                   "classificationRoot", "rankName", "rankID", "classificationsID", 
#                   "namebankID", "nameString")
#   child <- ifelsedata(childrenFlag, "children", 4)
#   ancestry <- ifelsedata(ancestryFlag, "ancestry", 4)
#   synonyms <- ifelsedata(synonymsFlag, "synonyms", 4)
#   refs <- ifelsedata(justificationsFlag, "citations", 4)
#   compact(list(data=out, children=child, ancestry=ancestry, synonyms=synonyms, refs=refs))
}

#' Function to parse xml data, decode strings, and return NULL if logical is zero.
#' @export
#' @keywords internal
ifelsedata <- function(x, y, z)
{
  if(x == 1){
    ldfast(getxmldata(obj=tt, node=y, todecode=z), convertvec=TRUE) 
  } else { NULL }
}