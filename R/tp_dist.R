#' Return all distribution records for for a taxon name with a given id.
#'
#' @export
#' @param id the taxon identifier code
#' @param key Your Tropicos API key; See [taxize-authentication] 
#' for help on authentication
#' @param ... Curl options passed on to [crul::HttpClient]
#' @return List of two [tibble::tibble]s, one named "location", and one "reference".
#' @references http://services.tropicos.org/help?method=GetNameDistributionsXml
#'
#' @examples \dontrun{
#' # Query using a taxon name Id
#' out <- tp_dist(id = 25509881)
#' ## just location data
#' head(out[['location']])
#' ## just reference data
#' head(out[['reference']])
#' }

tp_dist <- function(id, key=NULL, ...) {
  id <- as.numeric(as.character(id))
  if (!inherits(id, "numeric")) {
    stop("You must supply a numeric taxon name id")
  }

  url = sprintf('http://services.tropicos.org/Name/%s/Distributions', id)
	key <- getkey(key, "TROPICOS_KEY")
  args <- tc(list(format = 'json', apikey = key))
  tt <- tp_GET(url, args, ...)
  out <- jsonlite::fromJSON(tt, FALSE)
  getdata <- function(x, which) data.frame(x[[which]])
  locs <- dt2df(lapply(out, getdata, which = "Location"), idcol = FALSE)
  names(locs) <- tolower(names(locs))
  refs <- dt2df(lapply(out, getdata, which = "Reference"), idcol = FALSE)
  names(refs) <- tolower(names(refs))

  nested_list_df_to_tibbles(list(location = locs, reference = refs))
}

#' Return all distribution records for for a taxon name with a given id.
#'
#' Function name changed to tp_dist.
#'
#' @export
#' @keywords internal
#' @param ... ignored
#' @rdname tp_namedistributions-deprecated
tp_namedistributions <- function(...) {
  .Deprecated("tp_dist", "taxize", "Function name changed. See tp_dist", "tp_namedistributions")
}
