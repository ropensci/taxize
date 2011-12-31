#' Retrieve all taxa names or TSNs downstream in hierarchy from given TSN.
#' @import XML RCurl plyr
#' @param taxonConceptID the EOL page identifier (character)
#' @param json return in json format (TRUE or FALSE), if FALSE, returns XML
#' @param usekey use your API key or not (TRUE or FALSE)
#' @param returntype one of list of dataframe (character)
#' @param url The EOL url for the function (should be left to default).
#' @param key Your EOL API key; loads from .Rprofile.
#' @details Can't seem to get json format results along with specifiying an API key,
#'    so if you use json your key is not specified at the moment
#' @return List or dataframe of XXXX.
#' @export
#' @examples \dontrun{
#' eol_hierarchy('34345893')
#' eol_hierarchy('34345893', json = 'TRUE')
#' }
eol_hierarchy <- 

function(taxonConceptID, json = FALSE, usekey = FALSE, returntype = 'list',
  url = 'http://www.eol.org/api/hierarchy_entries/1.0/',
  key = getOption("EOLApi", stop("need an API key for Encyclopedia of Life"))) 
{
  if (json == TRUE) {
      urlget <- paste(url, taxonConceptID, '.json', sep="")
      searchresults <- fromJSON(urlget)
      } 
  else {
    if (usekey == TRUE) {usekey_ <- paste('?key=', key, sep='')} else {usekey_ <- NULL}
    urlget <- paste(url, taxonConceptID, usekey_, sep="")
    xmlout <- getURL(urlget)
    searchresults <- xmlToList(xmlTreeParse(xmlout))
    }
  
  if(returntype == 'list') { searchresults  } else
    {  ldply(searchresults, function(x) as.data.frame(x))  }  
}