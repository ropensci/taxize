#' Synonyms search
#' @export
#' @template worms_id
#' @examples \dontrun{
#' worms_synonyms(ids=733271)
#' }
worms_synonyms <- function(ids=NULL, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  upiface <- worms_update_iface(update_iface)
  if(!is.null(upiface)) worms_iface <- iface
  fxn <- worms_get_fxn('getAphiaSynonymsByID')
  res <- fxn(AphiaID = ids, server = server, .opts = opts)
  do.call(rbind, lapply(res, function(y) data.frame(unclass(y), stringsAsFactors = FALSE)))
}