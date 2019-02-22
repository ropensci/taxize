#' Return summary data a taxon name with a given id.
#'
#' @export
#' @param id the taxon identifier code
#' @param key Your Tropicos API key; See \code{\link{taxize-authentication}}
#' for help on authentication
#' @param ... Curl options passed on to \code{\link[crul]{verb-GET}}
#' @return A data.frame.
#' @examples \dontrun{
#' tp_summary(id = 25509881)
#' tp_summary(id = 2700851)
#' tp_summary(id = 24900183)
#' }

tp_summary <- function(id, key = NULL, ...) {
  url <- sprintf('http://services.tropicos.org/Name/%s', id)
	key <- getkey(key, "TROPICOS_KEY")

  args <- tc(list(apikey = key, format = 'json'))
  tt <- tp_GET(url, args, ...)
  res <- jsonlite::fromJSON(tt, FALSE)
  typespec <- data.frame(res$TypeSpecimens, stringsAsFactors = FALSE)
  df <- data.frame(res[!names(res) %in% "TypeSpecimens"], stringsAsFactors = FALSE)
  if (NROW(typespec) > 0) df <- cbind(df, typespec)
  stats::setNames(df, tolower(names(df)))
}

tp_GET <- function(url, query, ...) {
  cli <- crul::HttpClient$new(url = url, headers = tx_ual, opts = list(...))
  res <- cli$get(query = query)
  res$raise_for_status()
  if (grepl("exception occurred", res$parse("UTF-8"), ignore.case = TRUE)) {
    stop("500 - a server error occurred, try again later")
  }
  res$parse("UTF-8")
}
