#' Get PESI distributions from GUIDs
#' @export
#' @param guid One or more PESI GUIDs
#' @param opts Options passed on to \code{SSOAP::.SOAP} for curl debugging.
#' @param iface PESI SOAP interface. By default uses the cached version in taxize.
#' @param ... Further args passed on to SSOAP
#' @examples \dontrun{
#' pesi_distributions(guid='A0433E13-D7B5-49F2-86BA-A1777364C559')
#' pesi_distributions(guid=c('A0433E13-D7B5-49F2-86BA-A1777364C559',
#'                    '66374558-8F9A-4833-AEC8-490DAAC76024'))
#' }
pesi_distributions <- function(guid=NULL, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.eu-nomen.eu/portal/soap.php?p=soap'
  if(!is.null(iface)) pesi_iface <- iface
  fxn <- pesi_get_fxn('getPESIDistributionsByGUID')
  res <- lapply(guid, fxn, server = server, .opts = opts, .convert=FALSE, ...)
  do.call(rbind.fill, Map(parse_pesi_data, res, iter=guid))
}
