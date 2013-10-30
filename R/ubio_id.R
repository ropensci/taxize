#' Search uBio by namebank ID.
#' 
#' @import httr XML RCurl plyr
#' @param namebankID (string) - uBio namebank ID
#' @param keyCode Your uBio API key; loads from .Rprofile. If you don't have 
#'    one, obtain one at http://www.ubio.org/index.php?pagename=form.
#' @param callopts Parameters passed on to httr::GET call.
#' @return A list of four data.frame's, one for the name itself, one for synonyms, 
#' one for vernacular names, and one for citations.
#' @examples \dontrun{
#' ubio_id(namebankID = 2483153)
#' }
#' @export
ubio_id <- function(namebankID = NULL, keyCode = NULL, callopts=list())
{
  url <- "http://www.ubio.org/webservices/service.php"
  keyCode <- getkey(keyCode, "ubioApiKey")
  args <- compact(list(
    'function' = 'namebank_object', namebankID = namebankID, keyCode = keyCode))
  tmp <- GET(url, query=args, callopts)
  stop_for_status(tmp)
  tt <- content(tmp)
  
  toget <- c("namebankID", "nameString", "fullNameString", "packageID", 
             "packageName", "basionymUnit", "rankID", "rankName")
  temp <- lapply(toget, function(x) sapply(xpathApply(tt, paste("/results/", x, sep="")), xmlValue))
  temp[2:3] <- sapply(temp[2:3], base64Decode)
  out <- data.frame(do.call(cbind, temp))
  names(out) <- c("namebankID", "nameString", "fullNameString", "packageID", 
                  "packageName", "basionymUnit", "rankID", "rankName")
  
  syns <- getxmldata(obj=tt, node="homotypicSynonyms", todecode=2:3)
  verns <- getxmldata(obj=tt, node="vernacularNames", todecode=2)
  cites <- getxmldata(obj=tt, node="citations", todecode=c(1,3,4))
  
  list(data=out, 
       synonyms=ldfast(syns, convertvec=TRUE), 
       vernaculars=ldfast(verns, convertvec=TRUE), 
       citations=ldfast(cites, convertvec=TRUE))
}

#' Function to parse xml data and decode strings
#' @export
#' @keywords internal
getxmldata <- function(obj, node, todecode){
  tmp <- getNodeSet(obj, sprintf("/results/%s", node))[[1]]
  tmp2 <- xpathApply(tmp, sprintf("//%s", node), fun=xmlToList)[[1]]
  lapply(tmp2, function(x){
    x[todecode] <- sapply(x[todecode], RCurl::base64Decode)
    x
  })
}