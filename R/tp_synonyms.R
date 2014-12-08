#' Return all synonyms for a taxon name with a given id.
#'
#' @import XML RCurl jsonlite plyr
#' @export
#'
#' @param id the taxon identifier code
#' @param key Your Tropicos API key; loads from .Rprofile.
#' @param callopts Further args passed on to httr::GET
#' @return List or dataframe.
#' @examples \dontrun{
#' tp_synonyms(id = 25509881)
#' }

tp_synonyms <- function(id, key = NULL, callopts=list())
{
  url = sprintf('http://services.tropicos.org/Name/%s/Synonyms', id)
	key <- getkey(key, "tropicosApiKey")
  args <- taxize_compact(list(apikey= key, format = 'json'))
  tmp <- GET(url, query=args, callopts)
  stop_for_status(tmp)
  tmp2 <- content(tmp, as = "text")
  res <- jsonlite::fromJSON(tmp2, FALSE)

  if(names(res[[1]])[[1]] == "Error"){
    nonedf <- data.frame(nameid="no syns found",scientificname="no syns found",scientificnamewithauthors="no syns found",family="no syns found",stringsAsFactors = FALSE)
    list(accepted=nonedf, synonyms=nonedf)
  } else
  {
    dat <- lapply(res, function(x) lapply(x, data.frame, stringsAsFactors = FALSE))
    accepted <- dat[[1]]$AcceptedName
    df <- do.call(rbind.fill, lapply(dat, "[[", "SynonymName"))
    synonyms <- df[!duplicated.data.frame(df), ]
    names(accepted) <- tolower(names(accepted))
    names(synonyms) <- tolower(names(synonyms))
    list(accepted=accepted, synonyms=synonyms)
  }
}
