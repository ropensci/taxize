#' Retrieve all taxa names or TSNs downstream in hierarchy from given TSN.
#' 
#' @import RCurl plyr RJSONIO
#' @param taxonConceptID the EOL page identifier (character)
#' @param common_names Return common names or not (defaults to returning them, 
#' 		given commonnames=0 if not)
#' @param synonyms Return synonyms or not (defaults to returning them, 
#' 		given synonyms=0 if not)
#' @param usekey use your API key or not (TRUE or FALSE)
#' @param returntype one of "list" of "data.frame" (character)
#' @param url The EOL url for the function (should be left to default).
#' @param key Your EOL API key; loads from .Rprofile.
#' @details It's possible to return JSON or XML with the EOL API. However, 
#' 		this function only returns JSON for now. 
#' @return List or dataframe of XXXX.
#' @export
#' @examples \dontrun{
#' eol_hierarchy(taxonConceptID='39153621', returntype="data.frame")
#' eol_hierarchy(taxonConceptID='39153621', usekey=T, returntype="data.frame")
#' }
eol_hierarchy <- function(taxonConceptID, common_names = NULL, synonyms = NULL,
	usekey = FALSE, returntype = 'data.frame',
  url = 'http://www.eol.org/api/hierarchy_entries/1.0/',
  key = getOption("EOLApi", stop("need an API key for Encyclopedia of Life"))) 
{
	if(!is.null(common_names)){common_names<-"common_names=0"}else{common_names<-"common_names=1"}
	if(!is.null(synonyms)){synonyms<-"synonyms=0"}else{synonyms<-"synonyms=1"}
  if(usekey == TRUE){usekey_<-paste('key=',key,sep='')}else{usekey_<-NULL}
	query <- paste("?", paste(compact(list(common_names,synonyms,usekey_)), collapse="&"),sep="")
	urlget <- paste(url, taxonConceptID, '.json', query, sep="")
  searchresults <- fromJSON(urlget)
  
  if(returntype == 'list') { searchresults  } else
    if(returntype == 'data.frame'){  
    	ldply(searchresults$ancestors, function(x) as.data.frame(x))  
    } else  
    	stop("returntype must be one of 'list' or 'data.frame'")
}