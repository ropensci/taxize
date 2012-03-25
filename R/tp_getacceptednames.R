#' Return all accepted names for a taxon name with a given id.
#' @import XML RCurl RJSONIO plyr
#' @param id the taxon identifier code 
#' @param format return in json or xml format (defaults to json)
#' @param output raw = json or xml; or df = data.frame 
#' @param url The Tropicos url for the function (should be left to default).
#' @param key Your Tropicos API key; loads from .Rprofile.
#' @return List or dataframe.
#' @export
#' @examples \dontrun{
#' tp_getacceptednames(id = 25503923)
#' tp_getacceptednames(id = 25503923, output = 'raw')
#' }
tp_getacceptednames <- function(id, format = 'json', output = 'df',
  url = 'http://services.tropicos.org/Name/',
  key = getOption("tropicoskey", stop("need an API key for Tropicos"))) 
{
  if (format == 'json') {
    urlget <- paste(url, id, '/AcceptedNames?apikey=', key, '&format=json', sep="")
    message(urlget)
    searchresults <- fromJSON(urlget)
    } 
  else {
    urlget <- paste(url, id, '/AcceptedNames?apikey=', key, '&format=xml', sep="")
    message(urlget)
    xmlout <- getURL(urlget)
    searchresults <- xmlToList(xmlTreeParse(xmlout))
    }
  if(output == 'df') { 
    getdata <- function(x) {
      syn <- ldply(x[[1]])
      syn$category <- rep("Synonym", nrow(syn))
      acc <- ldply(x[[2]])
      acc$category <- rep("Accepted", nrow(acc))
      ref <- ldply(x[[3]])
      ref$category <- rep("Reference", nrow(ref))
      temp <- rbind(syn, acc, ref)
      names(temp)[1:2] <- c('variable','value')
      temp
    }
    ldply(searchresults, getdata)
  } else { searchresults }
}
# http://services.tropicos.org/Name/25503923/AcceptedNames?apikey=f3e499d4-1519-42c9-afd1-685a16882f5a&format=xml