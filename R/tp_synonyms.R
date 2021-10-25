#' Return all synonyms for a taxon name with a given id.
#'
#' @export
#' @param id the taxon identifier code
#' @param key Your Tropicos API key; See [taxize-authentication] 
#' for help on authentication
#' @param ... Curl options passed on to [crul::HttpClient]
#' @return List or [tibble::tibble].
#' @examples \dontrun{
#' tp_synonyms(id = 25509881)
#' }

tp_synonyms <- function(id, key = NULL, ...) {
  url = sprintf('http://services.tropicos.org/Name/%s/Synonyms', id)
	key <- getkey(key, "TROPICOS_KEY")
  args <- tc(list(apikey = key, format = 'json'))
  tt <- tp_GET(url, args, ...)
  res <- jsonlite::fromJSON(tt, FALSE)

  if (names(res[[1]])[[1]] == "Error") {
    nonedf <- data.frame(nameid = "no syns found", scientificname = "no syns found",
                         scientificnamewithauthors = "no syns found",
                         family = "no syns found", stringsAsFactors = FALSE)
    list(accepted = nonedf, synonyms = nonedf)
  } else {
    dat <- lapply(res, function(x) lapply(x, data.frame, stringsAsFactors = FALSE))
    accepted <- dat[[1]]$AcceptedName
    df <- dt2df(lapply(dat, "[[", "SynonymName"), idcol = FALSE)
    synonyms <- df[!duplicated.data.frame(df), ]
    names(accepted) <- tolower(names(accepted))
    names(synonyms) <- tolower(names(synonyms))
    nested_list_df_to_tibbles(list(accepted = accepted, synonyms = synonyms))
  }
}
