#' Get sources/references by ID
#' @export
#' @template worms_id
#' @examples \dontrun{
#' worms_sources(ids=1080)
#' worms_sources(ids=278241)
#' worms_sources(ids=c(1080,278241))
#' }
worms_sources <- function(ids=NULL, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  if(!is.null(iface)) worms_iface <- iface
  fxn <- worms_get_fxn('getSourcesByAphiaID')
  res <- lapply(ids, fxn, server = server, .opts = opts)
  names(res) <- ids
  parse_data_byname(res)
#   do.call(rbind, lapply(res, function(y) data.frame(unclass(y), stringsAsFactors = FALSE)))
}