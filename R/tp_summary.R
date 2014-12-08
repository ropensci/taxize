#' Return summary data a taxon name with a given id.
#'
#' @import httr jsonlite
#' @export
#'
#' @param id the taxon identifier code
#' @param key Your Tropicos API key; loads from .Rprofile.
#' @param callopts Curl options.
#' @return A data.frame.
#' @examples \dontrun{
#' tp_summary(id = 25509881)
#' tp_summary(id = 2700851)
#' tp_summary(id = 24900183)
#' }

tp_summary <- function(id, key = NULL, callopts=list())
{
  url <- sprintf('http://services.tropicos.org/Name/%s', id)
	key <- getkey(key, "tropicosApiKey")

  args <- taxize_compact(list(apikey = key, format = 'json'))
  tmp <- GET(url, query = args, callopts)
  stop_for_status(tmp)
  tmp2 <- content(tmp, as = "text")
  res <- jsonlite::fromJSON(tmp2, FALSE)
  typespec <- data.frame(res$TypeSpecimens, stringsAsFactors = FALSE)
  df <- data.frame(res[!names(res) %in% "TypeSpecimens"], stringsAsFactors = FALSE)
  if(NROW(typespec) > 0) df <- cbind(df, typespec)
  setNames(df, tolower(names(df)))
}
