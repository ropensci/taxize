#' Return summary data a taxon name with a given id.
#' 
#' @import XML RCurl RJSONIO plyr
#' @param id the taxon identifier code 
#' @param format return in json or xml format (defaults to json)
#' @param output raw = json or xml; or df = data.frame 
#' @param key Your Tropicos API key; loads from .Rprofile.
#' @param verbose Print messages (default) or not, logical
#' @return List or dataframe.
#' @examples \dontrun{
#' tp_summary(id = 25509881)
#' tp_summary(id = 25509881, output = 'raw')
#' }
#' @export
tp_summary <- function(id, format = 'json', output = 'df', key = NULL, verbose=TRUE) 
{
  url = 'http://services.tropicos.org/Name/'
	key <- getkey(key, "tropicosApiKey")
  if (format == 'json') {
      urlget <- paste(url, id, '?apikey=', key, '&format=json', sep="")
      mssg(verbose, urlget)
      searchresults <- fromJSON(urlget)
      } 
  else {
    urlget <- paste(url, id, '?apikey=', key, '&format=xml', sep="")
    mssg(verbose, urlget)
    xmlout <- getURL(urlget)
    searchresults <- xmlToList(xmlTreeParse(xmlout))
    }
  if(output == 'df') { ldply(searchresults, function(x) x[[1]]) } else
    { searchresults }
}