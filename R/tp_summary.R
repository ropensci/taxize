#' Return summary data a taxon name with a given id.
#' 
#' @import XML RCurl RJSONIO plyr
#' @param id the taxon identifier code 
#' @param format return in json or xml format (defaults to json)
#' @param output raw = json or xml; or df = data.frame 
#' @param url The Tropicos url for the function (should be left to default).
#' @param key Your Tropicos API key; loads from .Rprofile.
#' @return List or dataframe.
#' @examples \dontrun{
#' tp_summary(id = 25509881)
#' tp_summary(id = 25509881, output = 'raw')
#' }
#' @export
tp_summary <- function(id, format = 'json', output = 'df',
  url = 'http://services.tropicos.org/Name/', key = NULL) 
{
	key <- getkey(key)
  if (format == 'json') {
      urlget <- paste(url, id, '?apikey=', key, '&format=json', sep="")
      message(urlget)
      searchresults <- fromJSON(urlget)
      } 
  else {
    urlget <- paste(url, id, '?apikey=', key, '&format=xml', sep="")
    message(urlget)
    xmlout <- getURL(urlget)
    searchresults <- xmlToList(xmlTreeParse(xmlout))
    }
  if(output == 'df') { ldply(searchresults, function(x) x[[1]]) } else
    { searchresults }
}