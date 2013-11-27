#' Return all distribution records for for a taxon name with a given id.
#' 
#' @import XML RCurl RJSONIO plyr
#' @param id the taxon identifier code 
#' @param key Your Tropicos API key; loads from .Rprofile. Or you can passin your
#' key in this arg.
#' @param callopts Further args passed on to httr::GET
#' @return List of two data.frame's, one named "location", and one "reference".
#' @references \url{http://services.tropicos.org/help?method=GetNameDistributionsXml}
#' @examples \dontrun{
#' # Query using a taxon name Id
#' out <- tp_dist(id = 25509881)
#' ## just location data
#' head(out[['location']])
#' ## just reference data
#' head(out[['reference']])
#' }
#' @export
tp_dist <- function(id, key=NULL, callopts=list())
{
  id <- as.numeric(as.character(id))
  if(!inherits(id, "numeric"))
    stop("You must supply a numeric taxon name id")
  
  url = sprintf('http://services.tropicos.org/Name/%s/Distributions', id)
	key <- getkey(key, "tropicosApiKey")
  args <- compact(list(format='json', apikey=key))
  tt <- GET(url, query=args, callopts)
  stop_for_status(tt)
  out <- content(tt)
  getdata <- function(x, which) data.frame(x[[which]])
  locs <- do.call(rbind.fill, lapply(out, getdata, which="Location"))
  refs <- do.call(rbind.fill, lapply(out, getdata, which="Reference"))
  
  list(location = locs, reference = refs)
}