#' Get PESI GUID from scientific names
#' @import SSOAP XML
#' @export
#' @param scientific One of more scientific names
#' @param opts Options passed on to \code{SSOAP::.SOAP} for curl debugging.
#' @param iface PESI SOAP interface. By default uses the cached version in taxize.
#' @param ... Further args passed on to SSOAP
#' 
#' @examples \dontrun{
#' pesi_guid(scientific='Ternatea vulgaris')
#' pesi_guid(scientific='Holcus lanatus')
#' pesi_guid(scientific=c('Ternatea vulgaris','Carcharhinus altimus','Carcharhinus galapagensis'))
#' }
pesi_guid <- function(scientific=NULL, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.eu-nomen.eu/portal/soap.php?p=soap'
  if(!is.null(iface)) pesi_iface <- iface
  fxn <- pesi_get_fxn('getGUID')
  res <- lapply(scientific, fxn, server = server, .opts = opts, .convert=FALSE, ...)
  vapply(res, parse_pesi_xml, FUN.VALUE = character(1))
}
