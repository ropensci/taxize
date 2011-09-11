# eol_hierarchy.R

eol_hierarchy <- 
# Args:
#   taxonConceptID: the EOL page identifier (character)
#   json: return in json format (TRUE or FALSE), if FALSE, returns XML
#   usekey: use your API key or not (TRUE or FALSE)
#   returntype: one of list of dataframe (character)
# Notes: 
#   -Can't seem to get json format results along with specifiying an API key,
#   so if you use json your key is not specified at the moment
# Examples:
#   eol_hierarchy('34345893')
#   eol_hierarchy('34345893', json = 'TRUE')
# Returns: list or dataframe

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
  
  if(returntype = 'list') { return( searchresults ) } else
    { return( ldply(searchresults, function(x) as.data.frame(x)) ) }  
}
