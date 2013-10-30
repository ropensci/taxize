#' Search uBio by namebank ID.
#' 
#' @import httr XML RCurl plyr
#' @param hierarchiesID you must include the hierarchiesID (ClassificationBankID) 
#'    to receive the classification synonyms
#' @param keyCode Your uBio API key; loads from .Rprofile. If you don't have 
#'    one, obtain one at http://www.ubio.org/index.php?pagename=form.
#' @param callopts Parameters passed on to httr::GET call.
#' @return A data.frame.
#' @examples \dontrun{
#' ubio_synonyms(hierarchiesID = 4091702)
#' }
#' @export
ubio_synonyms <- function(hierarchiesID = NULL, keyCode = NULL, callopts=list())
{
  url <- "http://www.ubio.org/webservices/service.php"
  keyCode <- getkey(keyCode, "ubioApiKey")
  args <- compact(list(
    'function' = 'classification_list', hierarchiesID = hierarchiesID, keyCode = keyCode))
  tmp <- GET(url, query=args, callopts)
  stop_for_status(tmp)
  tt <- content(tmp)
  out <- getxmldata2(obj=tt, node="value", todecode=2:3)
  df <- ldfast(out, convertvec=TRUE)
  df[df == "\x9e\x89\xde"] <- "none"
  df
}

#' Function to parse xml data and decode strings
#' @export
#' @keywords internal
getxmldata2 <- function(obj, node, todecode){
  tmp <- getNodeSet(obj, sprintf("/results/%s", node))
  tmp2 <- lapply(tmp, xmlToList)
  tmp2 <- lapply(tmp2, function(x){
    ss <- sapply(x, is.null)
    x[ss] <- "none"
    x
  })
  lapply(tmp2, function(x){
    x[todecode] <- sapply(x[todecode], RCurl::base64Decode)
    x
  })
}