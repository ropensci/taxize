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
  hierarchiesID <- as.numeric(as.character(hierarchiesID))
  if(!inherits(hierarchiesID, "numeric"))
    stop("hierarchiesID must by a numeric")
    
  url <- "http://www.ubio.org/webservices/service.php"
  keyCode <- getkey(keyCode, "ubioApiKey")
  args <- compact(list(
    'function' = 'synonym_list', hierarchiesID = hierarchiesID, keyCode = keyCode))
  tmp <- GET(url, query=args, callopts)
  stop_for_status(tmp)
  tt <- content(tmp)
  out <- getxml_syns(obj=tt, todecode=c(5,6,12))
  df <- data.frame(out)
  df
}

getxml_syns <- function(obj, todecode){
  tmp <- xpathApply(obj, "//results", fun=xmlToList)
  tmp2 <- unlist(tmp)
  tmp2 <- lapply(tmp2, function(x){
    ss <- sapply(x, is.null)
    x[ss] <- "none"
    x
  })
  tmp2[todecode] <- sapply(tmp2[todecode], RCurl::base64Decode)
  tmp2 <- tmp2[-c(1:3)]
  names(tmp2) <- tolower(c('classificationTitleID','classificationTitle','classificationDescription',
                   'classificationRoot','rankName','rankID','classificationsID','namebankID',
                   'nameString'))
  tmp2
}