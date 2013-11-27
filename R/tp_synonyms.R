#' Return all synonyms for a taxon name with a given id.
#' 
#' @import XML RCurl RJSONIO plyr
#' @param id the taxon identifier code 
#' @param key Your Tropicos API key; loads from .Rprofile.
#' @param verbose Verbose or not
#' @return List or dataframe.
#' @export
#' @examples \dontrun{
#' tp_synonyms(id = 25509881)
#' }
tp_synonyms <- function(id, key = NULL, verbose=TRUE)
{
  url = 'http://services.tropicos.org/Name/'
	key <- getkey(key, "tropicosApiKey")
  urlget <- paste(url, id, '/Synonyms?apikey=', key, '&format=json', sep="")
  mssg(verbose, urlget)
  searchresults <- RJSONIO::fromJSON(urlget)
  
  if(names(searchresults[[1]])[[1]] == "Error"){
    nonedf <- data.frame(NameId="no syns found",ScientificName="no syns found",ScientificNameWithAuthors="no syns found",Family="no syns found")
    list(accepted=nonedf, synonyms=nonedf)
  } else
  {  
    dat <- lapply(searchresults, function(x) lapply(x, data.frame))
    accepted <- dat[[1]]$AcceptedName
    df <- do.call(rbind.fill, lapply(dat, "[[", "SynonymName"))
    synonyms <- df[!duplicated.data.frame(df), ]
    names(accepted) <- tolower(names(accepted))
    names(synonyms) <- tolower(names(synonyms))
    list(accepted=accepted, synonyms=synonyms)
  }
}