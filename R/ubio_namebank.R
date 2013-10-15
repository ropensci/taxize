#' Search uBio namebank.
#' 
#' @import XML RCurl plyr
#' @param searchName (string) - term to search within name string
#' @param searchAuth (string) - term to search within name authorship
#' @param searchYear (string) - term to search within name year
#' @param order (string) - (name or namebankID) field by which the results will 
#' be sorted (default is namebankID)
#' @param sci (int) - (sci, vern, or all) type of results to be returned 
#' (default is all)
#' @param vern (int) - (limit 1000) maximum number of results to be returned 
#' (default is 1000)
#' @param keyCode Your uBio API key; loads from .Rprofile. If you don't have 
#'    one, obtain one at http://www.ubio.org/index.php?pagename=form.
#' @details Can't seem to get json format results along with specifiying an 
#'    API key, so if you use json your key is not specified at the moment
#' @return List or dataframe of XXXX.
#' @examples \dontrun{
#' ubio_namebank(searchName = 'elephant', sci = 1, vern = 0)
#' ubio_namebank(searchName = 'Astragalus aduncus', sci = 1, vern = 0)
#' }
#' @export
ubio_namebank <- function(searchName = NULL, searchAuth = NULL, searchYear=NULL, 
		order = NULL, sci = NULL, vern = NULL, keyCode = NULL) 
{
	url = "http://www.ubio.org/webservices/service.php"
	keyCode <- getkey(keyCode, "ubioApiKey")
	args <- 
		compact(list('function' = 'namebank_search', searchName = searchName, searchAuth = searchAuth,
								 searchYear = searchYear, order = order,
								 sci = sci, vern = vern, keyCode = keyCode))
	temp <- xmlParse(getForm(url, .params=args))
	toget <- c("namebankID", "nameString", "fullNameString", "packageID", 
						 "packageName", "basionymUnit", "rankID", "rankName")
	temp2 <- lapply(toget, function(x) sapply(xpathApply(temp, paste("//", x, sep="")), xmlValue))
  temp2[2:3] <- sapply(temp2[2:3], base64Decode)
	out <- data.frame(do.call(cbind, temp2))
	names(out) <- c("namebankID", "nameString", "fullNameString", "packageID", 
									"packageName", "basionymUnit", "rankID", "rankName")
	out
}