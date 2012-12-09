#' Given the identifier for a data object, return all metadata about the object.
#' 
#' @import RCurl plyr RJSONIO
#' @param id The EOL data object identifier (character)
#' @param usekey use your API key or not (TRUE or FALSE)
#' @param returntype one of "list" of "data.frame" (character)
#' @param url The EOL url for the function (should be left to default).
#' @param key Your EOL API key; loads from .Rprofile.
#' @details It's possible to return JSON or XML with the EOL API. However, 
#' 		this function only returns JSON for now. 
#' @return List or dataframe.
#' @examples \dontrun{
#' eol_dataobjects(id="d72801627bf4adf1a38d9c5f10cc767f")
#' eol_dataobjects(id="21929584")
#' }
#' @export
eol_dataobjects <- function(id, returntype = 'data.frame', 
	url = 'http://www.eol.org/api/data_objects/1.0/', usekey = TRUE, key = NULL)
{
	key <- getkey(key, "EOL")
	if(usekey == TRUE){usekey_<-paste('key=',key,sep='')}else{usekey_<-NULL}
	key2 <- paste("?", paste(compact(usekey_), collapse="&"), sep="")
	urlget <- paste(url, id, '.json', key2, sep="")
	message(urlget)
	searchresults <- fromJSON(urlget)
	
	if(returntype == 'list') { searchresults  } else
		if(returntype == 'data.frame'){  
			message("not sure how to parse this yet, sorry for the wait")
			searchresults
		} else  
			stop("returntype must be one of 'list' or 'data.frame'")
}