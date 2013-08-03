#' Search for pages in EOL database using a taxonconceptID.
#' 
#' @import RCurl plyr RJSONIO
#' @param taxonconceptID The taxonconceptID (numeric), which is also the page 
#' 		number.
#' @param usekey Use your API key or not (TRUE or FALSE)
#' @param returntype One of "list" of "data.frame" (character)
#' @param key Your EOL API key; loads from .Rprofile, or you can specify the 
#' 		key manually the in the function call.
#' @details It's possible to return JSON or XML with the EOL API. However, 
#' 		this function only returns JSON for now. 
#' @return JSON list object, or data.frame.
#' @examples \dontrun{
#' pageid <- eol_search('Pomatomus')$id[1]
#' out <- eol_pages(taxonconceptID=pageid)
#' eol_hierarchy(out[out$nameAccordingTo == "NCBI Taxonomy", "identifier"])
#' eol_hierarchy(out[out$nameAccordingTo == "Integrated Taxonomic Information 
#'    System (ITIS)", "identifier"])
#' }
#' @export
eol_pages <- function(taxonconceptID, usekey = FALSE, returntype = 'data.frame', 
                      key = NULL) 
{     
  url = 'http://eol.org/api/pages/1.0/'
	key <- getkey(key, "EOL")
	if(usekey == TRUE){usekey_<-paste('?key=',key,sep='')}else{usekey_<-NULL}
	urlget <- paste(url, taxonconceptID, '.json', usekey_, sep="")
	message(urlget)
	searchresults <- fromJSON(urlget)
	
	if(returntype == 'list') { searchresults  } else
		if(returntype == 'data.frame'){  
			ldply(searchresults$taxonConcepts, function(x) as.data.frame(x))  
		} else  
			stop("returntype must be one of 'list' or 'data.frame'")
}