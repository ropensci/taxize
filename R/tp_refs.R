#' Return all reference records for for a taxon name with a given id.
#'
#' @export
#' @param id the taxon identifier code
#' @param key Your Tropicos API key; See [taxize-authentication]
#' for help on authentication
#' @param ... Curl options passed on to [crul::HttpClient]
#' @return List or dataframe.
#' @examples \dontrun{
#' tp_refs(id = 25509881)
#' }
tp_refs <- function(id, key = NULL, ...) {
  url = sprintf('http://services.tropicos.org/Name/%s/References', id)
	key <- getkey(key, "TROPICOS_KEY")

  args <- tc(list(apikey = key, format = 'json'))
  tt <- tp_GET(url, args, ...)
  res <- jsonlite::fromJSON(tt, FALSE)
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
#' @export
#' @keywords internal
#' @param ... ignored
#' @rdname tp_namereferences-deprecated
tp_namereferences <- function(...) {
  .Deprecated("tp_refs", "taxize", "Function name changed. See tp_refs", "tp_namereferences")
}
