#' Search for PESI scientific names and associated metadata.
#' @export
#' @param scientific One or more scientific names
#' @param opts Options passed on to \code{SSOAP::.SOAP} for curl debugging.
#' @param iface PESI SOAP interface. By default uses the cached version in taxize.
#' @param ... Further args passed on to SSOAP
#' @examples \dontrun{
#' pesi_search(scientific='Ternatea vulgaris')
#' pesi_search(scientific=c('Ternatea vulgaris','Carcharhinus altimus','Carcharhinus galapagensis'))
#' }
pesi_search <- function(scientific=NULL, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.eu-nomen.eu/portal/soap.php?p=soap'
  if(!is.null(iface)) pesi_iface <- iface
  endpt <- if(length(scientific) > 1) 'matchTaxa' else 'matchTaxon'
  fxn <- pesi_get_fxn(endpt)
  res <- lapply(scientific, fxn, server = server, .opts = opts, .convert=FALSE, ...)
  parsefxn <- switch(endpt, matchTaxa=parse_taxa, matchTaxon=parse_pesi_data)
  do.call(rbind.fill, Map(parsefxn, res, iter=scientific))
}
