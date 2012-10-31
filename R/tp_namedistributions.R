#' Return all distribution records for for a taxon name with a given id.
#' 
#' @import XML RCurl RJSONIO plyr
#' @param id the taxon identifier code 
#' @param format return in json or xml format (defaults to json)
#' @param output raw = json or xml; or df = data.frame 
#' @param url The Tropicos url for the function (should be left to default).
#' @param key Your Tropicos API key; loads from .Rprofile.
#' @return List or dataframe.
#' @examples \dontrun{
#' # Raw json or xml
#' tp_namedistributions(id = 25509881, output = 'raw')
#' 
#' # Output as data.frame
#' df <- tp_namedistributions(id = 25509881)
#' df[df$category %in% 'Location',] # just location data
#' df[df$category %in% 'Reference',] # just reference data
#' }
#' @export
tp_namedistributions <- function(id, format = 'json', output = 'df',
  url = 'http://services.tropicos.org/Name/', key = NULL) 
{
	key <- getkey(key, "tropicos")
  if (format == 'json') {
    urlget <- paste(url, id, '/Distributions?apikey=', key, '&format=json', sep="")
    message(urlget)
    searchresults <- fromJSON(urlget)
    } 
  else {
    urlget <- paste(url, id, '/Distributions?apikey=', key, '&format=xml', sep="")
    message(urlget)
    xmlout <- getURL(urlget)
    searchresults <- xmlToList(xmlTreeParse(xmlout))
    }
  if(output == 'df') { 
    getdata <- function(x) {
      loc <- ldply(x[[1]])
      loc$category <- rep("Location", nrow(loc))
      ref <- ldply(x[[2]])
      ref$category <- rep("Reference", nrow(ref))
      temp <- rbind(loc, ref)
      names(temp)[1:2] <- c('variable','value')
      temp$variable <- as.factor(temp$variable)
      temp$category <- as.factor(temp$category)
      temp
    }
    ldply(searchresults, getdata)
  } else { searchresults }
}