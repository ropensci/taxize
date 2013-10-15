#' Search for pages in EOL database using a taxonconceptID.
#' 
#' @import httr plyr
#' @param taxonconceptID The taxonconceptID (numeric), which is also the page 
#' 		number.
#' @param iucn Include the IUCN Red List status object (Default: False)
#' @param images Limits the number of returned image objects (values 0 - 75)
#' @param videos Limits the number of returned video objects (values 0 - 75)
#' @param sounds Limits the number of returned sound objects (values 0 - 75)
#' @param maps Limits the number of returned map objects (values 0 - 75)
#' @param text Limits the number of returned text objects (values 0 - 75)
#' @param subject 'overview' (default) to return the overview text (if exists), a 
#'    pipe | delimited list of subject names from the list of EOL accepted subjects 
#'    (e.g. TaxonBiology, FossilHistory), or 'all' to get text in any subject. Always 
#'    returns an overview text as a first result (if one exists in the given context).
#' @param licenses A pipe | delimited list of licenses or 'all' (default) to get objects 
#'    under any license. Licenses abbreviated cc- are all Creative Commons licenses. 
#'    Visit their site for more information on the various licenses they offer.
#' @param details Include all metadata for data objects. (Default: False)
#' @param common_names Return all common names for the page's taxon (Default: False)
#' @param syonyms Return all synonyms for the page's taxon (Default: False)
#' @param references Return all references for the page's taxon (Default: False)
#' @param vetted If 'vetted' is given a value of '1', then only trusted content will 
#'    be returned. If 'vetted' is '2', then only trusted and unreviewed content will 
#'    be returned (untrusted content will not be returned). The default is to return all 
#'    content. (Default: False)
#' @param cache_ttl The number of seconds you wish to have the response cached.
#' @param returntype One of "list" of "data.frame" (character)
#' @param key Your EOL API key; loads from .Rprofile, or you can specify the 
#' 		key manually the in the function call.
#' @param callopts Further args passed on to GET.
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

eol_pages <- function(taxonconceptID, iucn=NULL, images=NULL, videos=NULL, sounds=NULL, 
                      maps=NULL, text=NULL, subject=NULL, licenses=NULL, details=NULL,
                      common_names=NULL, synonyms=NULL, references=NULL, vetted=NULL,
                      cache_ttl=NULL, returntype='data.frame', key = NULL, callopts=list())
{     
  url <- 'http://eol.org/api/pages/1.0/'
	key <- getkey(key, "EOL")
  args <- compact(list(iucn=iucn,images=images,videos=videos,sounds=sounds, 
                       maps=maps,text=text,subject=subject,licenses=licenses,
                       details=details,common_names=common_names,synonyms=synonyms,
                       references=references,vetted=vetted,cache_ttl=cache_ttl))
  urlget <- paste(url, taxonconceptID, '.json', sep="")
  tt <- GET(urlget, query=args, callopts)
  stop_for_status(tt)
  searchresults <- content(tt)
	
	if(returntype == 'list') { searchresults  } else
		if(returntype == 'data.frame'){  
			ldply(searchresults$taxonConcepts, function(x) as.data.frame(x))  
		} else  
			stop("returntype must be one of 'list' or 'data.frame'")
}