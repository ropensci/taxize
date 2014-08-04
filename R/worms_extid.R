#' Get external ID from Worms ID
#' @export
#' @template worms_id
#' @param type External ID source to get ID for. One of ncbi (default), tsn, bold, eol,
#' dyntaxa, fishbase, iucn or lsid.
#' @return Character class with ID and attributess for found or not, the source name, and a URL for
#' the taxon with the external source. No URL is given for lsid.
#' @examples \dontrun{
#' worms_extid(ids=1080, type='ncbi')
#' worms_extid(ids=278241, type='tsn')
#' worms_extid(ids=278241, type='iucn')
#' worms_extid(ids=282981, type='fishbase')
#' worms_extid(ids=127186, type='bold')
#' worms_extid(ids=278241, type='eol')
#' worms_extid(ids=278241, type='lsid')
#' worms_extid(ids=127186, type='dyntaxa')
#' }
worms_extid <- function(ids=NULL, type='ncbi', opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  if(!is.null(iface)) worms_iface <- iface
  fxn <- worms_get_fxn('getExtIDbyAphiaID')
  res <- fxn(AphiaID = ids, type = type, server = server, .opts = opts)
  res <- if(length(res)==0) NA else res
  clazz <- switch(type, ncbi='uid', tsn='tsn', bold='bold', eol='eol', dyntaxa='dyntaxa', fishbase='fishbase', iucn='iucn', lsid='lsid')
  attr(res, "match") <- if(length(res)==0) 'not found' else 'found'
  attr(res, "uri") <- get_uri(type, res)
  class(res) <- clazz
  return(res)
}
