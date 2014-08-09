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
#' Each WoRMS record (row in the output of most \code{worms_} functions) has the same
#' output columns:
#' 
#' \itemize{
#'  \item AphiaID: unique and persistent identifier within WoRMS. Primary key in the database.
#'  \item url: HTTP URL to the AphiaRecord
#'  \item scientificname: the full scientific name without authorship
#'  \item authority: the authorship information for the scientificname formatted according to the 
#'  conventions of the applicable nomenclaturalCode
#'  \item rank: the taxonomic rank of the most specific name in the scientificname
#'  \item status: the status of the use of the scientificname as a label for a taxon. Requires 
#'  taxonomic opinion to define the scope of a taxon
#'  \item unacceptreason: the reason why a scientificname is unaccepted
#'  \item valid_AphiaID: the AphiaID (for the scientificname) of the currently accepted taxon
#'  \item valid_name: the scientificname of the currently accepted taxon
#'  \item valid_authority: the authorship information for the scientificname of the currently 
#'  accepted taxon
#'  \item kingdom: the full scientific name of the kingdom in which the taxon is classified
#'  \item phylum: the full scientific name of the phylum or division in which the taxon is classified
#'  \item class: the full scientific name of the class in which the taxon is classified
#'  \item order: the full scientific name of the order in which the taxon is classified
#'  \item family: the full scientific name of the family in which the taxon is classified
#'  \item genus: the full scientific name of the genus in which the taxon is classified
#'  \item citation: a bibliographic reference for the resource as a statement indicating how this 
#'  record should be cited (attributed) when used
#'  \item lsid: LifeScience Identifier. Persistent GUID for an AphiaID
#'  \item isMarine: a boolean flag indicating whether the taxon is a marine organism, i.e. can be 
#'  found in/above sea water. Possible values: 0/1/NULL
#'  \item isBrackish: a boolean flag indicating whether the taxon occurrs in brackish habitats. 
#'  Possible values: 0/1/NULL
#'  \item isFreshwater: a boolean flag indicating whether the taxon occurrs in freshwater 
#'  habitats, i.e. can be found in/above rivers or lakes. Possible values: 0/1/NULL
#'  \item isTerrestrial: a boolean flag indicating the taxon is a terrestial organism, i.e. occurrs 
#'  on land as opposed to the sea. Possible values: 0/1/NULL
#'  \item isExtinct: a flag indicating an extinct organism. Possible values: 0/1/NUL
#'  \item match_type: Type of match. Possible values: exact/like/phonetic/near_1/near_2
#'  \item modified: The most recent date-time in GMT on which the resource was changed
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

#' get function from ssoap defintion
#' 
#' @export
#' @keywords internal
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
#     do.call(rbind, lapply(y, function(z) data.frame(inputid=y[[1]]$AphiaID, unclass(z), stringsAsFactors = FALSE)))
    do.call(rbind, lapply(y, function(z) data.frame(inputid=slot(y[[1]], 'AphiaID'), t(sapply(slotNames(z), function(x) slot(z, x))), stringsAsFactors = FALSE)))
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


worms_parse_xml <- function(z, aphiaid, which="getAphiaChildrenByID")
{
  which <- if(which %in% c('getAphiaChildrenByID','getAphiaRecords','getAphiaRecordsByNames','getAphiaRecordsByVernacular','getAphiaRecordsByDate','matchAphiaRecordsByNames')) '//item' else '//return'
  st <- xmlParse( z$content )
  ns <- c(xmlns='xsi="http://www.w3.org/2001/XMLSchema-instance"')
  nodes <- getNodeSet(st, which, namespaces = ns)
  if(length(nodes) == 0) 
    nodes <- getNodeSet(st, '//return', namespaces = ns)
  out <- lapply(nodes, function(x){
    if(!is.null(xmlToList(x)[['nil']])){ data.frame(noresults=NA, stringsAsFactors = FALSE) } else {
      if(length(getNodeSet(x, "item")) == 0){
        extract_it(x)
      } else {
        tmp <- getNodeSet(x, 'item')
        do.call(rbind.fill, lapply(tmp, extract_it))
      }
    }
  })
  df <- data.frame(inputid=aphiaid, do.call(rbind.fill, out), stringsAsFactors = FALSE)
  df$.attrs <- NULL
  df
}

extract_it <- function(x){
  rr <- xmlToList(x)
  data.frame(lapply(rr, function(x) x['text'][[1]]), stringsAsFactors = FALSE)
}
