#' This function will return ClassificationBankIDs (hierarchiesIDs) that refer to the
#' given NamebankID
#'
#' @import XML
#' @export
#' @param namebankID (charcter) NameBank identifier you wish to search for in
#'    ClassificationBank
#' @param classificationTitleID Include if you only which to search within a
#'    particular classification
#' @param keyCode Your uBio API key; loads from .Rprofile. If you don't have
#'    one, obtain one at http://www.ubio.org/index.php?pagename=form.
#' @param ... Parameters passed on to \code{\link[httr]{GET}}
#' @return A data.frame with columns classificationBankID, classificationTitleID, and
#' classificationTitle
#' @examples \dontrun{
#' ubio_classification_search(namebankID = 3070378)
#' }

ubio_classification_search <- function(namebankID = NULL, classificationTitleID = NULL,
  keyCode = NULL, ...) {

  url <- "http://www.ubio.org/webservices/service.php"
  keyCode <- getkey(keyCode, "ubioApiKey")
  args <- tc(list(
    'function' = 'classificationbank_search', namebankID = namebankID,
    classificationTitleID = classificationTitleID, keyCode = keyCode))
  tmp <- GET(url, query = args, ...)
  stop_for_status(tmp)
  tt <- content(tmp)
  toget <- c("classificationBankID", "classificationTitleID", "classificationTitle")
  temp <- lapply(toget, function(x) sapply(xpathApply(tt, paste("//results//seniorNames//", x, sep = "")), xmlValue))
  temp[[3]] <- sapply(temp[[3]], function(z) rawToChar(openssl::base64_decode(z)), USE.NAMES = FALSE)
  out <- data.frame(do.call(cbind, temp), stringsAsFactors = FALSE)
  setNames(out, c("hierarchiesid", "classificationtitleid", "classificationtitle"))
}
