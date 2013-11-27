#' Retrieve the taxonomic hierarchy from given EOL taxonID.
#' 
#' @import httr plyr
#' @param taxonid the EOL page identifier (character)
#' @param common_names Return common names or not (defaults to returning them, 
#' 		give commonnames=0 if not)
#' @param synonyms Return synonyms or not (defaults to returning them, 
#' 		give synonyms=0 if not)
#' @param key Your EOL API key; loads from .Rprofile.
#' @param callopts Further args passed on to GET.
#' @details It's possible to return JSON or XML with the EOL API. However, 
#' 		this function only returns JSON for now. 
#' @return List or dataframe of results.
#' @examples \dontrun{
#' # Using get_eolid
#' eol_hierarchy(get_eolid(sciname='Poa annua'))
#' }
#' @export
eol_hierarchy <- function(taxonid, common_names = NULL, synonyms = NULL,
  key = NULL, callopts=list()) 
{
  # if NA input, return NA
  if(is.na(taxonid)){ NA } else
  {
    url = 'http://www.eol.org/api/hierarchy_entries/1.0/'
    key <- getkey(key, "eolApiKey")
    urlget <- paste(url, taxonid, '.json', sep="")
    args <- compact(list(common_names=common_names, synonyms=synonyms))
    tt <- GET(urlget, query=args, callopts)
    stop_for_status(tt)
    res <- content(tt)
    if(length(res$ancestors)==0){
      sprintf("No hierarchy information for %s", taxonid)
    } else
    {
      tmp <- do.call(rbind.fill, lapply(res$ancestors, data.frame))[,c('taxonID','scientificName','taxonRank')]
      names(tmp) <- tolower(names(tmp))
      tmp
    }
  }
}