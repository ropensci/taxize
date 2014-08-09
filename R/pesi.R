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

parse_pesi_xml <- function(z){
  res <- xmlParse(z$content)
  ns <- c(xmlns='xsi="http://www.w3.org/2001/XMLSchema-instance"')
  xpathApply(res, '//return', xmlValue, namespaces = ns)[[1]]
}

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

#' Get PESI common (vernacular) names from GUIDs
#' @export
#' @param guid One or more PESI GUIDs
#' @param opts Options passed on to \code{SSOAP::.SOAP} for curl debugging.
#' @param iface PESI SOAP interface. By default uses the cached version in taxize.
#' @param ... Further args passed on to SSOAP
#' @examples \dontrun{
#' pesi_name_common(guid='urn:lsid:marinespecies.org:taxname:105838')
#' pesi_name_common(guid='67C3CC9C-624A-40C5-B63A-AB880E0300D1')
#' pesi_name_common(guid=c('urn:lsid:marinespecies.org:taxname:105787','
#'                  urn:lsid:marinespecies.org:taxname:105790'))
#' }
pesi_name_common <- function(guid=NULL, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.eu-nomen.eu/portal/soap.php?p=soap'
  if(!is.null(iface)) pesi_iface <- iface
  fxn <- pesi_get_fxn('getPESIVernacularsByGUID')
  res <- lapply(guid, fxn, server = server, .opts = opts, .convert=FALSE, ...)
  do.call(rbind.fill, Map(parse_pesi_data, res, iter=guid))
}


#' Get PESI synonyms from GUIDs
#' @export
#' @param guid One or more PESI GUIDs
#' @param opts Options passed on to \code{SSOAP::.SOAP} for curl debugging.
#' @param iface PESI SOAP interface. By default uses the cached version in taxize.
#' @param ... Further args passed on to SSOAP
#' @examples \dontrun{
#' pesi_synonyms(guid='A0433E13-D7B5-49F2-86BA-A1777364C559')
#' pesi_synonyms(guid=c('A0433E13-D7B5-49F2-86BA-A1777364C559',
#'                    '66374558-8F9A-4833-AEC8-490DAAC76024'))
#' }
pesi_synonyms <- function(guid=NULL, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.eu-nomen.eu/portal/soap.php?p=soap'
  if(!is.null(iface)) pesi_iface <- iface
  fxn <- pesi_get_fxn('getPESISynonymsByGUID')
  res <- lapply(guid, fxn, server = server, .opts = opts, .convert=FALSE, ...)
  do.call(rbind.fill, Map(parse_pesi_data, res, iter=guid))
}


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
  do.call(rbind.fill, Map(parsefxn, res, iter=guid))
}

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
#   lapply(res, parse_pesi_data, which=endpt)
  iter <- switch(endpt, getPESIRecordsByVernacular=common, getPESIRecordByGUID=guid, getPESIRecords=scientific)
  do.call(rbind, Map(parse_pesi_data, res, iter=iter, which=endpt))
}

parse_pesi_data <- function(z, iter, which="getPESIRecords")
{
  which <- if(which %in% c('getPESIRecords','getPESIRecordsByVernacular','matchTaxa','matchTaxon')) '//item' else '//return'
  res <- xmlParse(z$content)
  ns <- c(xmlns='xsi="http://www.w3.org/2001/XMLSchema-instance"')
  nodes <- getNodeSet(res, which, namespaces = ns)
  out <- lapply(nodes, function(x){
    rr <- xmlToList(x)
    df <- data.frame(input=iter, lapply(rr, function(x) x['text'][[1]]), stringsAsFactors = FALSE)
    df$.attrs <- NULL
    df
  })
  do.call(rbind, out)
}

parse_taxa <- function(z, iter)
{
  res <- xmlParse(z$content)
  ns <- c(xmlns='xsi="http://www.w3.org/2001/XMLSchema-instance"')
  nodes <- getNodeSet(res, '//item//item', namespaces = ns)
  out <- lapply(nodes, function(x){
    rr <- xmlToList(x)
    df <- data.frame(input=iter, lapply(rr, function(x) x['text'][[1]]), stringsAsFactors = FALSE)
    df$.attrs <- NULL
    df
  })
  do.call(rbind, out)
}

pesi_gen_iface <- function(wsdl_url='http://www.eu-nomen.eu/portal/soap.php?wsdl=1', ...)
{
  w <- processWSDL(wsdl_url)
  genSOAPClientInterface(, w, ...)
}

pesi_get_fxn <- function(x) pesi_iface@functions[[x]]


#' Search Pan-European Species directories Infrastructure (PESI)
#'
#' PESI has a SOAP API. We store the machine generated API specification in the package as the
#' object \code{pesi_iface}. However, you can update the spec if you want using
#' \code{pesi_gen_iface}, then pass the output of that fxn to the \code{iface} parameter of
#' \code{pesi_*} functions.
#'
#' The following functions are available to interact with PESI:
#'
#' \itemize{
#'  \item pesi_gen_iface Generate new PESI SOAP API interface.
#'  \item pesi_name_common  Get complete taxonomic hierarchy from a PESI ID.
#'  \item pesi_name_scientific  Get scientific name from a PESI ID.
#'  \item pesi_records Get PESI records from a PESI ID, an external ID, a scientific name, a
#'  common name, or a start- or end-date.
#'  \item pesi_synonyms Get scientific name synonyms from a PESI ID.
#'  \item pesi_distributions Get distribution data
#'  \item pesi_guid Search for a GUID from a scientific name
#'  \item pesi_records Get PESI records
#'  \item pesi_search Search by scientific name
#' }
#'
#' @references \url{http://www.eu-nomen.eu/portal/index.php}
#' @name pesi
NULL
