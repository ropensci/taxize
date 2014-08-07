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
#' pesi_guid(scientific=c('Ternatea vulgaris','Carcharhinus altimus','Carcharhinus galapagensis'))
#' }
pesi_guid <- function(scientific=NULL, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.eu-nomen.eu/portal/soap.php?p=soap'
  if(!is.null(iface)) pesi_iface <- iface
  fxn <- pesi_get_fxn('getGUID')
  vapply(scientific, fxn, FUN.VALUE = character(1), server = server, .opts = opts, ..., USE.NAMES = FALSE)
}

#' Get PESI names from GUIDs
#' @export
#' @param guid One or more PESI GUIDs
#' @param opts Options passed on to \code{SSOAP::.SOAP} for curl debugging.
#' @param iface PESI SOAP interface. By default uses the cached version in taxize.
#' @param ... Further args passed on to SSOAP
#' @examples \dontrun{
#' pesi_name(guid='67C3CC9C-624A-40C5-B63A-AB880E0300D1')
#' pesi_name(guid=c('urn:lsid:marinespecies.org:taxname:105787','urn:lsid:marinespecies.org:taxname:105790'))
#' }
pesi_name <- function(guid=NULL, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.eu-nomen.eu/portal/soap.php?p=soap'
  if(!is.null(iface)) pesi_iface <- iface
  fxn <- pesi_get_fxn('getPESINameByGUID')
  vapply(guid, fxn, FUN.VALUE = character(1), server = server, .opts = opts, ..., USE.NAMES = FALSE)
}

#' Get PESI records from scientific names, common names, or GUIDs
#' @export
#' @param scientific One of more scientific names
#' @param common One of more scientific names
#' @param guid One or more PESI GUIDs
#' @param opts Options passed on to \code{SSOAP::.SOAP} for curl debugging.
#' @param iface PESI SOAP interface. By default uses the cached version in taxize.
#' @param ... Further args passed on to SSOAP
#' @examples \dontrun{
#' pesi_records(scientific='Ternatea vulgaris')
#' pesi_records(common='arctic tern')
#' pesi_records(common='great white shark')
#' pesi_records(guid='67C3CC9C-624A-40C5-B63A-AB880E0300D1')
#' pesi_records(guid=c('urn:lsid:marinespecies.org:taxname:105787','urn:lsid:marinespecies.org:taxname:105790'))
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
  lapply(res, parse_pesi_data, which=endpt)
}

parse_pesi_data <- function(z, which="getPESIRecords")
{
  which <- if(which %in% c('getPESIRecords','getPESIRecordsByVernacular')) '//item' else '//return'
  res <- xmlParse(z$content)
  ns <- c(xmlns='xsi="http://www.w3.org/2001/XMLSchema-instance"')
  nodes <- getNodeSet(res, which, namespaces = ns)
  out <- lapply(nodes, function(x){
    rr <- xmlToList(x)
    data.frame(lapply(rr, function(x) x['text'][[1]]), stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

pesi_gen_iface <- function(wsdl_url='http://www.eu-nomen.eu/portal/soap.php?wsdl=1', ...)
{
  w <- processWSDL(wsdl_url)
  genSOAPClientInterface(, w, ...)
}

pesi_get_fxn <- function(x) pesi_iface@functions[[x]]
