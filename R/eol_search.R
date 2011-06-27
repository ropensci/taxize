# eol_search.R

eol_search <- 
# Args:
#   terms: search terms (character)
#   json: return in json format (TRUE or FALSE), if FALSE, returns XML
#   usekey: use your API key or not (TRUE or FALSE)
# Notes: 
#   -Can't seem to get json format results along with specifiying an API key,
#   so if you use json your key is not specified at the moment
#   -The 'page' option is not currently implemented as it doesn't seem useful
# Examples:
#   eol_search('Homo')
#   eol_search('Salix', json = FALSE)

function(terms, json = FALSE, usekey = FALSE,
  url = 'http://www.eol.org/api/search/',
  key = getOption("EOLApi", stop("need an API key for Encyclopedia of Life"))) 
{
  if (json == TRUE) {
      urlget <- paste(url, terms, '.json', sep="")
      searchresults <- fromJSON(urlget)
      } 
    else {
      if (usekey == TRUE) {usekey_ <- paste('?key=', key, sep='')} else {usekey_ <- NULL}
      urlget <- paste(url, terms, usekey_, sep="")
      xmlout <- getURL(urlget)
      searchresults <- xmlToList(xmlTreeParse(xmlout))
      }
  return( searchresults )
}