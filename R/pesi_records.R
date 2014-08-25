#' Get PESI records from scientific names, common names, or GUIDs
#' @export
#' @param scientific One of more scientific names
#' @param common One of more scientific names
#' @param guid One or more PESI GUIDs
#' @param like Argument...
#' @param opts Options passed on to \code{SSOAP::.SOAP} for curl debugging.
#' @param iface PESI SOAP interface. By default uses the cached version in taxize.
#' @param ... Further args passed on to SSOAP
#' @examples \dontrun{
#' pesi_records(scientific='Ternatea vulgaris')
#' pesi_records(common='arctic tern')
#' pesi_records(common='great white shark')
#' pesi_records(guid='67C3CC9C-624A-40C5-B63A-AB880E0300D1')
#' pesi_records(guid=c('urn:lsid:marinespecies.org:taxname:105787',
#'                    'urn:lsid:marinespecies.org:taxname:105790'))
#' }
pesi_records <- function(scientific=NULL, common=NULL, guid=NULL, like=NULL, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.eu-nomen.eu/portal/soap.php?p=soap'
  if(!is.null(iface)) pesi_iface <- iface
  endpt <- if(!is.null(common)){
    'getPESIRecordsByVernacular'
  } else if (!is.null(guid)) {
    'getPESIRecordByGUID'
  } else {
    'getPESIRecords'
  }
  fxn <- pesi_get_fxn(endpt)
  res <- switch(endpt,
                getPESIRecordsByVernacular = lapply(common, fxn, server = server, .opts = opts, .convert = FALSE, ...),
                getPESIRecordByGUID = lapply(guid, fxn, server = server, .opts = opts, .convert = FALSE, ...),
                getPESIRecords = lapply(scientific, fxn, like = like, server = server, .opts = opts, .convert = FALSE, ...)
  )
  iter <- switch(endpt, getPESIRecordsByVernacular=common, getPESIRecordByGUID=guid, getPESIRecords=scientific)
  do.call(rbind, Map(parse_pesi_data, res, iter=iter, which=endpt))
}
