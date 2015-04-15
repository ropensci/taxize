#' Return all accepted names for a taxon name with a given id.
#'
#' @import httr plyr jsonlite
#' @export
#' @param id the taxon identifier code
#' @param key Your Tropicos API key; loads from .Rprofile.
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
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
	key <- getkey(key, "tropicosApiKey")
  args <- taxize_compact(list(apikey = key, format = 'json'))
  tmp <- GET(url, query = args, ...)
  stop_for_status(tmp)
  tmp2 <- content(tmp, as = "text")
  res <- jsonlite::fromJSON(tmp2, FALSE)

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

# getdata <- function(x) {
#   syn <- ldply(x[[1]])
#   syn$category <- rep("Synonym", nrow(syn))
#   acc <- ldply(x[[2]])
#   acc$category <- rep("Accepted", nrow(acc))
#   ref <- ldply(x[[3]])
#   ref$category <- rep("Reference", nrow(ref))
#   temp <- rbind(syn, acc, ref)
#   names(temp)[1:2] <- c('variable','value')
#   temp
# }

#' Return all accepted names for a taxon name with a given id.
#'
#' Function name changed to tp_accnames.
#'
#' @param id the taxon identifier code
#' @param format return in json or xml format (defaults to json)
#' @param output raw = json or xml; or df = data.frame
#' @param key Your Tropicos API key; loads from .Rprofile.
#' @export
#' @keywords internal
#' @rdname tp_acceptednames-deprecated
tp_acceptednames <- function(id, format = 'json', output = 'df', key = NULL) {
  .Deprecated("tp_accnames", "taxize", "Function name changed. See tp_accnames", "tp_acceptednames")
}
