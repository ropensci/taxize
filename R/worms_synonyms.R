#' Synonyms search of WoRMS data.
#' 
#' @export
#' @template worms_id
#' @examples \dontrun{
#' worms_synonyms(ids=733271)
#' worms_synonyms(ids=c(733271,125725,159283))
#' }
worms_synonyms <- function(ids=NULL, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  if(!is.null(iface)) worms_iface <- iface
  fxn <- worms_get_fxn('getAphiaSynonymsByID')
  res <- lapply(ids, fxn, server = server, .opts = opts)
  parse_data(res)
}
