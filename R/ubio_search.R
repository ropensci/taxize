#' This function will return NameBankIDs that match given search terms
#' 
#' @import httr XML RCurl plyr
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
#' @param callopts Parameters passed on to httr::GET call.
#' @return A data.frame.
#' @examples \dontrun{
#' ubio_search(searchName = 'elephant', sci = 1, vern = 0)
#' ubio_search(searchName = 'Astragalus aduncus', sci = 1, vern = 0)
#' }
#' @export
ubio_search <- function(searchName = NULL, searchAuth = NULL, searchYear=NULL, 
		order = NULL, sci = NULL, vern = NULL, keyCode = NULL, callopts=list()) 
{
	url = "http://www.ubio.org/webservices/service.php"
	keyCode <- getkey(keyCode, "ubioApiKey")
	args <- compact(list('function' = 'namebank_search', searchName = searchName, 
                       searchAuth = searchAuth,
	                     searchYear = searchYear, order = order,
	                     sci = sci, vern = vern, keyCode = keyCode))
	tmp <- GET(url, query=args, callopts)
	stop_for_status(tmp)
	tt <- content(tmp)
	toget <- c("namebankID", "nameString", "fullNameString", "packageID", 
						 "packageName", "basionymUnit", "rankID", "rankName")
	temp2 <- lapply(toget, function(x) sapply(xpathApply(tt, paste("//", x, sep="")), xmlValue))
  temp2[2:3] <- sapply(temp2[2:3], base64Decode)
	out <- data.frame(do.call(cbind, temp2))
	names(out) <- c("namebankID", "nameString", "fullNameString", "packageID", 
									"packageName", "basionymUnit", "rankID", "rankName")
	out
}