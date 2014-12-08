#' Search uBio by namebank ID.
#'
#' @import httr XML RCurl
#' @export
#' @param namebankID (character) uBio namebank ID
#' @param keyCode Your uBio API key; loads from .Rprofile. If you don't have
#'    one, obtain one at http://www.ubio.org/index.php?pagename=form.
#' @param callopts Parameters passed on to httr::GET call.
#' @return A list of four data.frame's, one for the name itself, one for synonyms,
#' one for vernacular names, and one for citations.
#' @examples \dontrun{
#' ubio_id(namebankID = 2483153)
#' ubio_id(namebankID = 105509)
#' ubio_id(namebankID = 2843601)
#' ubio_id(namebankID = 2478181)
#'
#' # Pass in curl options
#' library("httr")
#' ubio_id(namebankID = 2478181, callopts=verbose())
#' ubio_id(namebankID = 2478181, callopts=timeout(3))
#' }

ubio_id <- function(namebankID = NULL, keyCode = NULL, callopts=list())
{
  url <- "http://www.ubio.org/webservices/service.php"
  keyCode <- getkey(keyCode, "ubioApiKey")
  args <- taxize_compact(list(
    'function' = 'namebank_object', namebankID = namebankID, keyCode = keyCode))
  tmp <- GET(url, query=args, callopts)
  stop_for_status(tmp)
  tt <- content(tmp)

  toget <- c("namebankID", "nameString", "fullNameString", "packageID",
             "packageName", "basionymUnit", "rankID", "rankName")
  temp <- lapply(toget, function(x) sapply(xpathApply(tt, paste("/results/", x, sep="")), xmlValue))
  temp[2:3] <- sapply(temp[2:3], function(x){
    trybase64 <- tryCatch(base64Decode(x), error=function(e) e)
    if( is(trybase64, "error") ) "" else x
  })
  out <- data.frame(do.call(cbind, temp), stringsAsFactors = FALSE)
  names(out) <- c("namebankID", "nameString", "fullNameString", "packageID",
                  "packageName", "basionymUnit", "rankID", "rankName")
  out <- tolowerfxn(out)
  out <- if(all(temp == "")) NULL else out

  # check for existence
  if(length(xpathApply(tt, "//homotypicSynonyms")) == 0) { syns <- NULL } else {
    syns <- getxmldata(obj=tt, node="homotypicSynonyms", todecode=2:3)
    syns <- tolowerfxn(taxize_ldfast(syns, convertvec=TRUE))
  }

  if(length(xpathApply(tt, "//vernacularNames")) == 0) { verns <- NULL } else {
    verns <- getxmldata(obj=tt, node="vernacularNames", todecode=2)
    verns <- tolowerfxn(taxize_ldfast(verns, convertvec=TRUE))
  }

  if(length(xpathApply(tt, "//results//mappings")) == 0 && length(xpathApply(tt, "//results//citations")) == 0) { mappings <- cites <- NULL } else {
    checkmapping <- xpathApply(tt, "//results//mappings")
    if(length(checkmapping) == 0){
      cites <- getxmldata(obj=tt, node="citations", todecode=c(1,3,4))
      cites <- tolowerfxn(taxize_ldfast(cites, convertvec=TRUE))
      mappings <- NULL
    } else {
      mappings <- getxmldata(obj=tt, node="mappings")
      mappings <- tolowerfxn(taxize_ldfast(mappings, convertvec=TRUE))
      cites <- NULL
    }
  }

  list(data=out, synonyms=syns, vernaculars=verns, cites=cites, mappings=mappings)
}

getxmldata <- function(obj, node, todecode=NULL){
  tmp <- getNodeSet(obj, sprintf("/results/%s", node))[[1]]
  tmp2 <- xpathApply(tmp, sprintf("//%s", node), fun=xmlToList)[[1]]
  if(!is.null(todecode)){
    lapply(tmp2, function(x){
      x[todecode] <- sapply(x[todecode], base64Decode)
      x
    })
  } else { tmp2 }
}

tolowerfxn <- function(x){
  if(is.null(x)){ NULL } else {
    names(x) <- tolower(names(x))
    return( x )
  }
}
