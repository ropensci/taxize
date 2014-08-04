#' Get sources/references by ID
#' @export
#' @template worms_id
#' @examples \dontrun{
#' worms_sources(ids=1080)
#' worms_sources(ids=278241)
#' }
worms_sources <- function(ids=NULL, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  upiface <- worms_update_iface(update_iface)
  if(!is.null(upiface)) worms_iface <- iface
  fxn <- worms_get_fxn('getSourcesByAphiaID')
  res <- fxn(AphiaID = ids, server = server, .opts = opts)
  do.call(rbind, lapply(res, function(y) data.frame(unclass(y), stringsAsFactors = FALSE)))
}