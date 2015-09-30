#' Return all reference records for for a taxon name with a given id.
#'
#' @export
#'
#' @param id the taxon identifier code
#' @param key Your Tropicos API key; loads from .Rprofile.
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @return List or dataframe.
#' @examples \dontrun{
#' tp_refs(id = 25509881)
#' }
tp_refs <- function(id, key = NULL, ...) {
  url = sprintf('http://services.tropicos.org/Name/%s/References', id)
	key <- getkey(key, "tropicosApiKey")

  args <- tc(list(apikey = key, format = 'json'))
  tmp <- GET(url, query = args, ...)
  stop_for_status(tmp)
  tmp2 <- content(tmp, as = "text")
  res <- jsonlite::fromJSON(tmp2, FALSE)
  do.call(rbind.fill, lapply(res, function(x){
    x <- x$Reference
    names(x) <- tolower(names(x))
    data.frame(x, stringsAsFactors = FALSE)
  }))
}

#' Return all reference records for for a taxon name with a given id.
#'
#' Function name changed to tp_refs.
#'
#' @param id the taxon identifier code
#' @param format return in json or xml format (defaults to json)
#' @param output raw = json or xml; or df = data.frame
#' @param key Your Tropicos API key; loads from .Rprofile.
#' @param verbose Print messages (default) or not, logical
#' @export
#' @keywords internal
#' @rdname tp_namereferences-deprecated
tp_namereferences <- function(id, format = 'json', output = 'df', key = NULL, verbose=TRUE)
{
  .Deprecated("tp_refs", "taxize", "Function name changed. See tp_refs", "tp_namereferences")
}
