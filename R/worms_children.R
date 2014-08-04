#' Children search
#' @export
#' @template worms_id
#' @param offset Starting record number, when retrieving next chunk of (50) records. Default=1.
#' @param marine_only (logical) Include results from marine taxa only. Default: TRUE.
#' @examples \dontrun{
#' worms_children(ids=106135)
#' }
worms_children <- function(ids=NULL, offset=NULL, marine_only=1, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  upiface <- worms_update_iface(update_iface)
  if(!is.null(upiface)) worms_iface <- iface
  fxn <- worms_get_fxn('getAphiaChildrenByID')
  res <- fxn(AphiaID = ids, offset = offset, marine_only = marine_only, server = server, .opts = opts)
  do.call(rbind, lapply(res, function(y) data.frame(unclass(y), stringsAsFactors = FALSE)))
}