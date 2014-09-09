#' Given the identifier for a data object, return all metadata about the object.
#' 
#' @import RCurl plyr jsonlite
#' @export
#' @param id The EOL data object identifier (character)
#' @param usekey use your API key or not (TRUE or FALSE)
#' @param returntype one of "list" of "data.frame" (character)
#' @param key Your EOL API key; loads from .Rprofile.
#' @param verbose logical; If TRUE the actual taxon queried is printed on the 
#'    console.
#' @details It's possible to return JSON or XML with the EOL API. However, 
#' 		this function only returns JSON for now. 
#' @return List or dataframe (default).
#' @examples \donttest{
#' eol_dataobjects(id="d72801627bf4adf1a38d9c5f10cc767f")
#' eol_dataobjects(id="21929584")
#' }

eol_dataobjects <- function(id, returntype = 'data.frame', usekey = TRUE, key = NULL, verbose=TRUE)
{
  url = 'http://www.eol.org/api/data_objects/1.0/'
	key <- getkey(key, "eolApiKey")
	if(usekey == TRUE){usekey_<-paste('key=',key,sep='')}else{usekey_<-NULL}
	key2 <- paste("?", paste(compact(usekey_), collapse="&"), sep="")
	urlget <- paste(url, id, '.json', key2, sep="")
	mssg(verbose, urlget)
  tt <- GET(urlget)
  stop_for_status(tt)
  res <- content(tt, as = "text")    
  returntype <- match.arg(returntype, c('list','data.frame'))
  if(returntype == 'list') { jsonlite::fromJSON(res, FALSE)  } else {
    jsonlite::fromJSON(res)
  }	
}
