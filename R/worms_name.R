#' Get name from a WORMS id
#'
#' @export
#' @template worms_id
#' @examples \dontrun{
#' worms_name(ids=1080)
#' worms_name(ids=c(1080,22388,160281,123080,22388))
#' worms_name(ids=c(1080,22388,160281,123080,22388,106135,159283))
#' }
worms_name <- function(ids=NULL, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  if(!is.null(iface)) worms_iface <- iface
  fxn <- worms_get_fxn('getAphiaNameByID')
  vapply(ids, fxn, FUN.VALUE = character(1), server = server, .opts = opts)
}
