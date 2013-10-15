#' Retrieve the taxonomic hierarchy from given EOL taxonID.
#' 
#' @import httr plyr
#' @param taxonid the EOL page identifier (character)
#' @param common_names Return common names or not (defaults to returning them, 
#' 		give commonnames=0 if not)
#' @param synonyms Return synonyms or not (defaults to returning them, 
#' 		give synonyms=0 if not)
#' @param returntype one of "list" of "data.frame" (character)
#' @param key Your EOL API key; loads from .Rprofile.
#' @param callopts Further args passed on to GET.
#' @details It's possible to return JSON or XML with the EOL API. However, 
#' 		this function only returns JSON for now. 
#' @return List or dataframe of results.
#' @examples \dontrun{
#' pageid <- eol_search('Pomatomus')$id[1]
#' out <- eol_pages(taxonconceptID=pageid)
#' eol_hierarchy(taxonid=out[out$nameAccordingTo == "NCBI Taxonomy", "identifier"])
#' eol_hierarchy(out[out$nameAccordingTo == "Integrated Taxonomic Information 
#'    System (ITIS)", "identifier"])
#' }
#' @export
eol_hierarchy <- function(taxonid, common_names = NULL, synonyms = NULL,
	returntype = 'data.frame', key = NULL, callopts=list()) 
{
  url = 'http://www.eol.org/api/hierarchy_entries/1.0/'
	key <- getkey(key, "EOL")
	urlget <- paste(url, taxonid, '.json', sep="")
  args <- compact(list(common_names=common_names, synonyms=synonyms))
  
  tt <- GET(urlget, query=args, callopts)
  stop_for_status(tt)
  searchresults <- content(tt)
  
  if(returntype == 'list') { searchresults  } else
    if(returntype == 'data.frame'){  
    	ldply(searchresults$ancestors, function(x) as.data.frame(x))  
    } else  
    	stop("returntype must be one of 'list' or 'data.frame'")
}