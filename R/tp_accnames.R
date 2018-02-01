#' Return all accepted names for a taxon name with a given id.
#'
#' @export
#' @param id the taxon identifier code
#' @param key Your Tropicos API key; See \code{\link{taxize-authentication}} 
#' for help on authentication
#' @param ... Curl options passed on to \code{\link[crul]{HttpClient}}
#' @return List or dataframe.
#' @examples \dontrun{
#' tp_accnames(id = 25503923)
#' tp_accnames(id = 25538750)
#'
#' # No accepted names found
#' tp_accnames(id = 25509881)
#' }

tp_accnames <- function(id, key = NULL, ...) {
  url = sprintf('http://services.tropicos.org/Name/%s/AcceptedNames', id)
	key <- getkey(key, "TROPICOS_KEY")
  args <- tc(list(apikey = key, format = 'json'))
  tt <- tp_GET(url, args, ...)
  res <- jsonlite::fromJSON(tt, FALSE)

  if ("Error" %in% names(res[[1]])) {
    res[[1]]
  } else {
    vvv <- lapply(res, getdata)
    syns <- do.call(rbind.fill, lapply(vvv, "[[", "syn"))
    accs <- do.call(rbind.fill, lapply(vvv, "[[", "acc"))
    refs <- do.call(rbind.fill, lapply(vvv, "[[", "ref"))
    list(synonyms = syns, acceptednames = accs, reference = refs)
  }
}

getdata <- function(x) {
  syn <- data.frame(x$SynonymName, stringsAsFactors = FALSE)
  names(syn) <- tolower(names(syn))
  acc <- data.frame(x$AcceptedName, stringsAsFactors = FALSE)
  names(acc) <- tolower(names(acc))
  ref <- data.frame(x$Reference, stringsAsFactors = FALSE)
  names(ref) <- tolower(names(ref))
  list(syn = syn, acc = acc, ref = ref)
}

#' Return all accepted names for a taxon name with a given id.
#'
#' Function name changed to tp_accnames.
#'
#' @export
#' @keywords internal
#' @param ... ignored
#' @rdname tp_acceptednames-deprecated
tp_acceptednames <- function(...) {
  .Deprecated("tp_accnames", "taxize", "Function name changed. See tp_accnames", "tp_acceptednames")
}
