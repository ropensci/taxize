#' Return all distribution records for for a taxon name with a given id.
#'
#' @export
#'
#' @param id the taxon identifier code
#' @param key Your Tropicos API key; loads from .Rprofile. Or you can passin your
#' key in this arg.
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @return List of two data.frame's, one named "location", and one "reference".
#' @references \url{http://services.tropicos.org/help?method=GetNameDistributionsXml}
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
	key <- getkey(key, "tropicosApiKey")
  args <- compact(list(format = 'json', apikey = key))
  tt <- GET(url, query = args, ...)
  stop_for_status(tt)
  out <- content(tt)
  getdata <- function(x, which) data.frame(x[[which]])
  locs <- do.call(rbind.fill, lapply(out, getdata, which = "Location"))
  names(locs) <- tolower(names(locs))
  refs <- do.call(rbind.fill, lapply(out, getdata, which = "Reference"))
  names(refs) <- tolower(names(refs))

  list(location = locs, reference = refs)
}

#' Return all distribution records for for a taxon name with a given id.
#'
#' Function name changed to tp_dist.
#'
#' @param id the taxon identifier code
#' @param key Your Tropicos API key; loads from .Rprofile. Or you can passin your
#' key in this arg.
#' @param callopts Further args passed on to httr::GET
#' @export
#' @keywords internal
#' @rdname tp_namedistributions-deprecated
tp_namedistributions <- function(id, key=NULL, callopts=list())
{
  .Deprecated("tp_dist", "taxize", "Function name changed. See tp_dist", "tp_namedistributions")
}
