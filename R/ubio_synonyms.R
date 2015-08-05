#' Search uBio for taxonomic synonyms by hierarchiesID.
#'
#' @export
#' @param hierarchiesID you must include the hierarchiesID (ClassificationBankID)
#'    to receive the classification synonyms
#' @param keyCode Your uBio API key; loads from .Rprofile. If you don't have
#'    one, obtain one at http://www.ubio.org/index.php?pagename=form.
#' @param ... Parameters passed on to \code{\link[httr]{GET}}
#' @return A data.frame.
#' @examples \dontrun{
#' ubio_synonyms(hierarchiesID = 4091702)
#' ubio_synonyms(hierarchiesID = 2483153)
#' ubio_synonyms(hierarchiesID = 2465599)
#' ubio_synonyms(hierarchiesID = 1249021)
#' ubio_synonyms(hierarchiesID = 4069372)
#'
#' # Pass in curl options
#' library("httr")
#' ubio_synonyms(hierarchiesID = 4091702, config=verbose())
#' }

ubio_synonyms <- function(hierarchiesID = NULL, keyCode = NULL, ...) {
  hierarchiesID <- as.numeric(as.character(hierarchiesID))
  if (!inherits(hierarchiesID, "numeric")) {
    stop("hierarchiesID must by a numeric", call. = FALSE)
  }

  url <- "http://www.ubio.org/webservices/service.php"
  keyCode <- getkey(keyCode, "ubioApiKey")
  args <- tc(list(
    'function' = 'synonym_list', hierarchiesID = hierarchiesID, keyCode = keyCode))
  tmp <- GET(url, query = args, ...)
  stop_for_status(tmp)
  tt <- content(tmp)
  out <- getxml_syns(obj = tt, todecode = c(2, 3, 9))
  data.frame(out, stringsAsFactors = FALSE)
}

getxml_syns <- function(obj, todecode){
  toget <- c('classificationTitleID','classificationTitle','classificationDescription',
             'classificationRoot','rankName','rankID','classificationsID','namebankID',
             'nameString')
  tmp <- sapply(toget, function(x) xpathApply(obj, sprintf("//results//%s", x), xmlValue))
  tmp <- lapply(tmp, function(x){
    ss <- sapply(x, is.null)
    x[ss] <- "none"
    x
  })
  tmp[todecode] <- sapply(tmp[todecode], function(x) {
    if (nchar(x) == 0) {
      x
    } else {
      rawToChar(openssl::base64_decode(x))
    }
  })
  setNames(tmp, tolower(toget))
}
