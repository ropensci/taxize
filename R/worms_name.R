#' Get name from a WORMS id
#' @export
#' @import SSOAP
#' @template worms_id
#' @examples \dontrun{
#' worms_name(id=1080)
#' }
worms_name <- function(id=NULL, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  upiface <- worms_update_iface(update_iface)
  if(!is.null(upiface)) worms_iface <- iface
  fxn <- worms_get_fxn('getAphiaNameByID')
  fxn(AphiaID = id, server = server, .opts = opts)
}