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
#' For more information, the PESI home page \url{http://www.eu-nomen.eu/portal/webservices.php}, 
#' and description of each of the functions used internally within this package
#' \url{http://www.eu-nomen.eu/portal/soap.php}.
#'
#' @references \url{http://www.eu-nomen.eu/portal/index.php}
#' @name pesi
NULL

parse_pesi_xml <- function(z){
  res <- xmlParse(z$content)
  ns <- c(xmlns='xsi="http://www.w3.org/2001/XMLSchema-instance"')
  xpathApply(res, '//return', xmlValue, namespaces = ns)[[1]]
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

#' Generate PESI function interface.
#'
#' @export
#' @param wsdl_url URL for the PESI SOAP WSDL file
#' @param ... Further args passed on to \code{genSOAPClientInterface}
#' @return Returns invisibly a S4 object holding all functions to interact with WORMS.
#' @examples \dontrun{ 
#' # You can generate your own interface to PESI functions
#' out <- pesi_gen_iface()
#' out
#' 
#' # Then access them directly
#' out@@functions$getPESIRecords(scientificname='Salmo', like=FALSE)
#' 
#' # Or pass to the taxize functions in the iface parameter
#' head(pesi_records(scientific = "Salmo", iface = out))
#' }
pesi_gen_iface <- function(wsdl_url='http://www.eu-nomen.eu/portal/soap.php?wsdl=1', ...)
{
  w <- processWSDL(wsdl_url)
  genSOAPClientInterface(, w, ...)
}

pesi_get_fxn <- function(x) pesi_iface@functions[[x]]
