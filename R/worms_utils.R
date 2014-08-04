#' Search World Register of Marine Species (WoRMS)
#'
#' WORMS has a SOAP API. We store the machine generated API specification in the package. However,
#' you can update the spec if you want using \code{worms_update_iface}
#'
#' @references \url{http://www.marinespecies.org/}
#' @name worms
NULL

#' Generate WORMS intrface.
#'
#' @export
#' @param wsdl_url URL for the WORMS SOAP WSDL file
#' @param ... Further args passed on to \code{genSOAPClientInterface}
#' @return Returns invisibly a S4 object holding all functions to interact with WORMS.
#' @examples \dontrun{
#' out <- worms_gen_iface()
#' out
#' worms_records(scientific='Salmo', iface=out)
#' }
worms_gen_iface <- function(wsdl_url="http://www.marinespecies.org/aphia.php?p=soap&wsdl=1", ...)
{
  w <- processWSDL(wsdl_url)
  genSOAPClientInterface(, w, ...)
}

worms_get_fxn <- function(x) worms_iface@functions[[x]]

get_uri <- function(x, y){
  switch(x,
         ncbi=sprintf('http://www.ncbi.nlm.nih.gov/taxonomy/%s', y),
         tsn=sprintf('http://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=%s', y),
         bold=sprintf('http://www.boldsystems.org/index.php/Taxbrowser_Taxonpage?taxid=%s', y),
         eol=sprintf('http://eol.org/pages/%s/overview', y),
         dyntaxa=sprintf('http://www.dyntaxa.se/Taxon/Info/%s?changeRoot=True', y),
         fishbase=sprintf('http://www.fishbase.org/summary/%s', y),
         iucn=sprintf('http://www.iucnredlist.org/details/%s/0', y),
         lsid=NA,
  )
}
