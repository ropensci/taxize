#' Common names from ID
#' @export
#' @template worms_id
#' @examples \dontrun{
#' worms_common(ids=1080)
#' worms_common(ids=22388)
#' worms_common(ids=123080)
#' worms_common(ids=160281)
#' }
worms_common <- function(ids=NULL, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  upiface <- worms_update_iface(update_iface)
  if(!is.null(upiface)) worms_iface <- iface
  fxn <- worms_get_fxn('getAphiaVernacularsByID')
  res <- fxn(AphiaID = ids, server = server, .opts = opts)
  do.call(rbind, lapply(res, function(y) data.frame(unclass(y), stringsAsFactors = FALSE)))
}