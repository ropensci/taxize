#' Common names from WoRMS ID
#' 
#' @export
#' @template worms_id
#' @examples \dontrun{
#' worms_common(ids=1080)
#' worms_common(ids=22388)
#' worms_common(ids=123080)
#' worms_common(ids=160281)
#' worms_common(ids=c(1080,22388,160281,123080,22388))
#' }
worms_common <- function(ids=NULL, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  if(!is.null(iface)) worms_iface <- iface
  fxn <- worms_get_fxn('getAphiaVernacularsByID')
  res <- lapply(ids, fxn, server = server, .opts = opts)
  names(res) <- ids
  parse_data_byname(res)
}
