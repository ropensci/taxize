#' Return all reference records for for a taxon name with a given id.
#' @import XML RCurl RJSONIO plyr
#' @param id the taxon identifier code 
#' @param format return in json or xml format (defaults to json)
#' @param output raw = json or xml; or df = data.frame 
#' @param url The Tropicos url for the function (should be left to default).
#' @param key Your Tropicos API key; loads from .Rprofile.
#' @return List or dataframe.
#' @export
#' @examples \dontrun{
#' # Raw json or xml
#' tp_getnamereferences(id = 25509881, output = 'raw')
#' 
#' # Output as data.frame
#' tp_getnamereferences(id = 25509881)
#' }
tp_getnamereferences <- function(id, format = 'json', output = 'df',
  url = 'http://services.tropicos.org/Name/',
  key = getOption("tropicoskey", stop("need an API key for Tropicos"))) 
{
  if (format == 'json') {
    urlget <- paste(url, id, '/References?apikey=', key, '&format=json', sep="")
    message(urlget)
    searchresults <- fromJSON(urlget)
    } 
  else {
    urlget <- paste(url, id, '/References?apikey=', key, '&format=xml', sep="")
    message(urlget)
    xmlout <- getURL(urlget)
    searchresults <- xmlToList(xmlTreeParse(xmlout))
    }
  if(output == 'df') { 
    getdata <- function(x) {
      ref <- ldply(x[[1]])
      names(ref) <- c('variable','value')
      ref$variable <- as.factor(ref$variable)
      ref
    }
    ldply(searchresults, getdata)
  } else { searchresults }
}