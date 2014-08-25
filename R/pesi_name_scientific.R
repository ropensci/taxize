#' Get PESI scientific names from GUIDs
#' @export
#' @param guid One or more PESI GUIDs
#' @param opts Options passed on to \code{SSOAP::.SOAP} for curl debugging.
#' @param iface PESI SOAP interface. By default uses the cached version in taxize.
#' @param ... Further args passed on to SSOAP
#' @examples \dontrun{
#' pesi_name_scientific(guid='67C3CC9C-624A-40C5-B63A-AB880E0300D1')
#' pesi_name_scientific(guid=c('urn:lsid:marinespecies.org:taxname:105787','
#'                  urn:lsid:marinespecies.org:taxname:105790'))
#' }
pesi_name_scientific <- function(guid=NULL, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.eu-nomen.eu/portal/soap.php?p=soap'
  if(!is.null(iface)) pesi_iface <- iface
  fxn <- pesi_get_fxn('getPESINameByGUID')
  res <- lapply(guid, fxn, server = server, .opts = opts, .convert=FALSE, ...)
  vapply(res, parse_pesi_xml, FUN.VALUE = character(1))
}
