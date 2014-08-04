#' Search World Register of Marine Species (WoRMS)
#'
#' WORMS has a SOAP API. We store the machine generated API specification in the package as the 
#' object \code{worms_iface}. However, you can update the spec if you want using 
#' \code{worms_gen_iface}, then pass the output of that fxn to the \code{iface} parameter of 
#' \code{worms_*} functions.
#' 
#' The following functions are available to interact with WoRMS:
#' 
#' \itemize{
#'  \item worms_gen_iface Generate new WoRMS SOAP API interface.
#'  \item worms_children Get immediate children from a WoRMS ID.
#'  \item worms_common  Get common (vernacular) names from a WoRMS ID.
#'  \item worms_extid  Get external IDs from a WoRMS ID, E.g., get an NCBI identifier.
#'  \item worms_hierarchy  Get complete taxonomic hierarchy from a WoRMS ID.
#'  \item worms_name  Get scientific name from a WoRMS ID.
#'  \item worms_records Get WoRMS records from a WoRMS ID, an external ID, a scientific name, a 
#'  common name, or a start- or end-date.
#'  \item worms_sources Get sources (references) for taxa from a WoRMS ID.
#'  \item worms_synonyms Get scientific name synonyms from a WoRMS ID.
#' }
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

parse_data <- function(x){
  do.call(rbind, lapply(x, function(y) if(length(y)==1){
    data.frame(inputid=y[[1]]$AphiaID, unclass(y[[1]]), stringsAsFactors = FALSE)
  } else {
    do.call(rbind, lapply(y, function(z) data.frame(inputid=y[[1]]$AphiaID, unclass(z), stringsAsFactors = FALSE)))
  }
  ))
}

parse_data_byname <- function(x){
  tt <- list()
  for(i in seq_along(x)){  
    if(length(x[[i]])==1){
      tt[[i]] <- data.frame(inputid=names(x)[i], unclass(x[[i]][[1]]), stringsAsFactors = FALSE)
    } else {
      tt[[i]] <- do.call(rbind, lapply(x[[i]], function(z) data.frame(inputid=names(x)[i], unclass(z), stringsAsFactors = FALSE)))
    }
  }
  do.call(rbind, tt)
}