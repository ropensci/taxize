#' This function will return NameBankIDs that match given search terms
#'
#' THIS FUNCTION IS DEFUNCT.
#'
#' @rdname ubio_search-defunct
#' @export
#' @param searchName (character) - term to search within name string
#' @param searchAuth (character) - term to search within name authorship
#' @param searchYear (character) - term to search within name year
#' @param order (character) - (name or namebankID) field by which the results will
#' be sorted (default is namebankID)
#' @param sci (integer) - O (no) or 1 (yes; default) to include scientific name results
#' (default is all)
#' @param vern (integer) - O (no) or 1 (yes; default) to include common name (vernacular) results
#' @param keyCode (character) Your uBio API key; loads from .Rprofile. If you don't have
#'    one, obtain one at http://www.ubio.org/index.php?pagename=form.
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @return A data.frame.
#' @examples \dontrun{
#' ubio_search(searchName = 'elephant')
#' ubio_search(searchName = 'elephant', sci = 1, vern = 0)
#' ubio_search(searchName = 'Astragalus aduncus', sci = 1, vern = 0)
#' ubio_search(searchName = 'puma concolor', sci=1, vern=0)
#' }

ubio_search <- function(searchName = NULL, searchAuth = NULL, searchYear=NULL,
		order = NULL, sci = 1, vern = 1, keyCode = NULL, ...) {
  .Defunct(msg = "the uBio API is down, for good as far as we know")

  #check for both zeros in sci and vern
  if (sci == 0 && vern == 0) {
    stop("Either sci, vern, or both have to be 1")
  }

	url = "http://www.ubio.org/webservices/service.php"
	keyCode <- getkey(keyCode, "ubioApiKey")
	args <- tc(list('function' = 'namebank_search', searchName = searchName,
                       searchAuth = searchAuth,
	                     searchYear = searchYear, order = order,
	                     sci = sci, vern = vern, keyCode = keyCode))
	tmp <- GET(url, query = args, ...)
	stop_for_status(tmp)
	tt <- content(tmp)
  if (!sci == 1) {
    sciout <- NULL
  } else {
    scitoget <- c("namebankID", "nameString", "fullNameString", "packageID",
                  "packageName", "basionymUnit", "rankID", "rankName")
    temp2 <- lapply(scitoget, function(x) sapply(xpathApply(tt, paste("//scientificNames//", x, sep = "")), xmlValue))
    temp2[2:3] <- lapply(temp2[2:3], function(x) {
      unname(sapply(x, function(z) rawToChar(openssl::base64_decode(z))))
    })
    sciout <- data.frame(do.call(cbind, temp2), stringsAsFactors = FALSE)
    names(sciout) <- tolower(scitoget)
  }
  if (!vern == 1) {
    vernout <- NULL
  } else {
    verntoget <- c("namebankID", "nameString", "languageCode", "languageName",
      "packageID", "packageName", "namebankIDLink", "nameStringLink", "fullNameStringLink")
    temp2 <- lapply(verntoget, function(x) sapply(xpathApply(tt, paste("//vernacularNames//", x, sep = "")), xmlValue))
    temp2[c(2,8,9)] <- lapply(temp2[c(2,8,9)], function(x) {
      unname(sapply(x, function(z) rawToChar(openssl::base64_decode(z))))
    })
    vernout <- data.frame(do.call(cbind, temp2), stringsAsFactors = FALSE)
    names(vernout) <- tolower(verntoget)
  }
	list(scientific = sciout, vernacular = vernout)
}
