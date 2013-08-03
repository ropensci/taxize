#' Search for terms in EOL database.
#' 
#' @import RCurl plyr RJSONIO
#' @param terms search terms (character)
#' @param usekey use your API key or not (TRUE or FALSE)
#' @param returntype one of "list" of "data.frame" (character)
#' @param key Your EOL API key; loads from .Rprofile.
#' @details It's possible to return JSON or XML with the EOL API. However, 
#' 		this function only returns JSON for now. 
#' @return JSON list object, or data.frame.
#' @examples \dontrun{
#' eol_search('Homo')
#' eol_search('Salix')
#' eol_search(terms='Ursus americanus luteolus')
#' }
#' @export
eol_search <- function(terms, usekey = TRUE, returntype = 'data.frame', 
                       key = NULL) 
{     
  url = 'http://eol.org/api/search/'
	key <- getkey(key, "EOL")
	if(usekey == TRUE){usekey_<-paste('?key=',key,sep='')}else{usekey_<-NULL}
	query <- gsub("\\s", "+", terms)
	urlget <- paste(url, query, '.json', usekey_, sep="")
	message(urlget)
	searchresults <- fromJSON(urlget)
	
	if(returntype == 'list') { searchresults  } else
		if(returntype == 'data.frame'){  
			ldply(searchresults$results, function(x) as.data.frame(x))  
		} else  
			stop("returntype must be one of 'list' or 'data.frame'")
}