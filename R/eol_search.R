#' Search for terms in EOL database.
#' @import XML RCurl RJSONIO
#' @param terms search terms (character)
#' @param json return in json format (TRUE or FALSE), if FALSE, returns XML
#' @param usekey use your API key or not (TRUE or FALSE)
#' @param url The EOL url for the function (should be left to default).
#' @param key Your EOL API key; loads from .Rprofile.
#' @details Can't seem to get json format results along with specifiying an 
#'    API key, so if you use json your key is not specified at the moment.  
#'    Also, the 'page' option is not currently implemented as it doesn't seem 
#'    useful, so if you use json your key is not specified at the moment
#' @return XML or JSON object.
#' @export
#' @examples \dontrun{
#' eol_search('Homo')
#' eol_search('Salix')
#' }
eol_search <- 
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