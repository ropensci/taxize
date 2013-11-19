#' Return all synonyms for a taxon name with a given id.
#' 
#' @import XML RCurl RJSONIO plyr
#' @param id the taxon identifier code 
#' @param format return in json or xml format (defaults to json)
#' @param key Your Tropicos API key; loads from .Rprofile.
#' @param verbose Verbose or not
#' @return List or dataframe.
#' @export
#' @examples \dontrun{
#' tp_synonyms(id = 25509881)
#' tp_synonyms(id = 25509881, output = 'raw')
#' }
tp_synonyms <- function(id, format = 'json', key = NULL, verbose=TRUE)
{
  url = 'http://services.tropicos.org/Name/'
	key <- getkey(key, "tropicosApiKey")
  if (format == 'json') {
    urlget <- paste(url, id, '/Synonyms?apikey=', key, '&format=json', sep="")
    mssg(verbose, urlget)
    searchresults <- RJSONIO::fromJSON(urlget)
  } 
  else {
    urlget <- paste(url, id, '/Synonyms?apikey=', key, '&format=xml', sep="")
    mssg(verbose, urlget)
    xmlout <- getURL(urlget)
    searchresults <- xmlToList(xmlTreeParse(xmlout))
  }
  
  if(names(searchresults[[1]])[[1]] == "Error"){
    nonedf <- data.frame(NameId="no syns found",ScientificName="no syns found",ScientificNameWithAuthors="no syns found",Family="no syns found")
    list(accepted=nonedf, synonyms=nonedf)
  } else
  {  
    dat <- lapply(searchresults, function(x) lapply(x, data.frame))
    accepted <- dat[[1]]$AcceptedName
#     synonyms <- do.call(rbind.fill, lapply(dat, "[[", "SynonymName"))[,c('NameId','ScientificName','Family')]
    df <- do.call(rbind.fill, lapply(dat, "[[", "SynonymName"))
    synonyms <- df[!duplicated.data.frame(df), ]
    list(accepted=accepted, synonyms=synonyms)
  }
}