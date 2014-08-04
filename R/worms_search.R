#' Search World Register of Marine Species (WoRMS)
#'
#' WORMS has a SOAP API. We store the machine generated API specification in the package. However,
#' you can update the spec if you want using \code{worms_update_iface}
#'
#' @references \url{http://www.marinespecies.org/}
#' @name worms
#' @docType function
NULL

#' Get name from a WORMS id
#' @export
#' @import SSOAP
#' @template worms_id
#' @examples \dontrun{
#' worms_name(id=1080)
#' }
worms_name <- function(id=NULL, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  upiface <- worms_update_iface(update_iface)
  if(!is.null(upiface)) worms_iface <- iface
  fxn <- worms_get_fxn('getAphiaNameByID')
  fxn(AphiaID = id, server = server, .opts = opts)
}

#' Hierarchy search
#' @export
#' @template worms_id
#' @examples \dontrun{
#' worms_hierarchy(ids=733271)
#' }
worms_hierarchy <- function(ids=NULL, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  upiface <- worms_update_iface(update_iface)
  if(!is.null(upiface)) worms_iface <- iface
  fxn <- worms_get_fxn('getAphiaClassificationByID')
  res <- fxn(AphiaID = ids, server = server, .opts = opts)
  df <- data.frame(aphiaid=res@AphiaID, rank=res@rank, scientificname=res@scientificname, stringsAsFactors = FALSE)
  hier <- slot(res, "child")
  rbind(df, parse_hier(hier, c("AphiaID","rank","scientificname")))
}

parse_hier <- function(x, slotnames){
  out <- list()
  iter <- 1
  done <- NULL
  xplus <- x
  while(is.null(done)){
    iter <- iter+1
    vals <- sapply(slotnames, function(x) slot(xplus, name = x))
    out[[iter]] <- vals
    xplus <- xplus@child
    done <- if(!length(xplus@AphiaID)==0) NULL else "done"
  }
  tmp <- ldply(compact(out))
  names(tmp)[1] <- 'aphiaid'
  tmp
}

#' Children search
#' @export
#' @template worms_id
#' @param offset Starting record number, when retrieving next chunk of (50) records. Default=1.
#' @param marine_only (logical) Include results from marine taxa only. Default: TRUE.
#' @examples \dontrun{
#' worms_children(ids=106135)
#' }
worms_children <- function(ids=NULL, offset=NULL, marine_only=1, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  upiface <- worms_update_iface(update_iface)
  if(!is.null(upiface)) worms_iface <- iface
  fxn <- worms_get_fxn('getAphiaChildrenByID')
  res <- fxn(AphiaID = ids, offset = offset, marine_only = marine_only, server = server, .opts = opts)
  do.call(rbind, lapply(res, function(y) data.frame(unclass(y), stringsAsFactors = FALSE)))
}

#' Synonyms search
#' @export
#' @template worms_id
#' @examples \dontrun{
#' worms_synonyms(ids=733271)
#' }
worms_synonyms <- function(ids=NULL, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  upiface <- worms_update_iface(update_iface)
  if(!is.null(upiface)) worms_iface <- iface
  fxn <- worms_get_fxn('getAphiaSynonymsByID')
  res <- fxn(AphiaID = ids, server = server, .opts = opts)
  do.call(rbind, lapply(res, function(y) data.frame(unclass(y), stringsAsFactors = FALSE)))
}

#' Common names from ID
#' @export
#' @template worms_id
#' @examples \dontrun{
#' worms_common(ids=1080)
#' worms_common(ids=22388)
#' worms_common(ids=123080)
#' worms_common(ids=160281)
#' }
worms_common <- function(ids=NULL, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  upiface <- worms_update_iface(update_iface)
  if(!is.null(upiface)) worms_iface <- iface
  fxn <- worms_get_fxn('getAphiaVernacularsByID')
  res <- fxn(AphiaID = ids, server = server, .opts = opts)
  do.call(rbind, lapply(res, function(y) data.frame(unclass(y), stringsAsFactors = FALSE)))
}

#' Get sources/references by ID
#' @export
#' @template worms_id
#' @examples \dontrun{
#' worms_sources(ids=1080)
#' worms_sources(ids=278241)
#' }
worms_sources <- function(ids=NULL, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  upiface <- worms_update_iface(update_iface)
  if(!is.null(upiface)) worms_iface <- iface
  fxn <- worms_get_fxn('getSourcesByAphiaID')
  res <- fxn(AphiaID = ids, server = server, .opts = opts)
  do.call(rbind, lapply(res, function(y) data.frame(unclass(y), stringsAsFactors = FALSE)))
}

#' Get records by ID, scientific name, common name, date, worms id, or external id.
#' @export
#' @template worms_id
#' @param scientific (character) A scientific name.
#' @param common (character) A common name.
#' @param id (integer) A WoRMS (or Aphia) identifier.
#' @param extid (integer) External identifier.
#' @param like Add a '%'-sign added after the ScientificName (SQL LIKE function). Default=TRUE
#' @param type Type of exxternal identifier. Should have one of bold, dyntaxa, eol, fishbase,
#' iucn, lsid, ncbi, or tsn.
#' @param marine_only (logical) Include results from marine taxa only. Default: TRUE.
#' @param offset Starting record number, when retrieving next chunk of (50) records. Default=1.
#' @param startdate ISO 8601 formatted start date(time). Default=today(). 
#' i.e. 2014-08-04T15:57:54+00:00
#' @param enddate ISO 8601 formatted start date(time). Default=today(). 
#' i.e. 2014-08-04T15:57:54+00:00
#' @details Parameter \code{type} should be one of the following values:
#' \itemize{
#'  \item bold: Barcode of Life Database (BOLD) TaxID
#'  \item dyntaxa: Dyntaxa ID
#'  \item eol: Encyclopedia of Life (EoL) page identifier
#'  \item fishbase: FishBase species ID
#'  \item iucn: IUCN Red List Identifier
#'  \item lsid: Life Science Identifier
#'  \item ncbi: NCBI Taxonomy ID (Genbank)
#'  \item tsn: ITIS Taxonomic Serial Number 
#' }
#' @examples \dontrun{
#' worms_records(scientific='Salmo')
#' worms_records(scientific='Liopsetta glacialis')
#' worms_records(common='salmon')
#' worms_records(startdate='2014-06-01T00:00:00', enddate='2014-06-02T00:00:00')
#' worms_records(id=1080)
#' worms_records(extid=6830, type='ncbi')
#' }
worms_records <- function(scientific=NULL, common=NULL, id=NULL, extid=NULL, like=NULL, type=NULL,
  marine_only=1, offset=NULL, startdate=NULL, enddate=NULL, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  if(!is.null(iface)) worms_iface <- iface
  endpt <- if(!is.null(common)){
    'getAphiaRecordsByVernacular'
  } else if (!is.null(startdate)|!is.null(enddate)) {
    'getAphiaRecordsByDate'
  } else if(!is.null(extid)) {
    'getAphiaRecordByExtID'
  } else if(!is.null(id)){
    'getAphiaRecordByID'
  } else {
    if(length(scientific) > 1) 'getAphiaRecordsByNames' else 'getAphiaRecords'
  }
  fxn <- worms_get_fxn(endpt)
  res <- switch(endpt,
    getAphiaRecords = fxn(scientificname = scientific, like = like, fuzzy = 'false', marine_only = marine_only, offset = 'false', server = server, .opts = opts, ...),
    getAphiaRecordsByNames = fxn(scientificname = scientific, like = like, fuzzy = 'false', marine_only = marine_only, server = server, .opts = opts, ...),
    getAphiaRecordsByVernacular = fxn(vernacular = common, like = like, offset = offset, server = server, .opts = opts, ...),
    getAphiaRecordsByDate = fxn(startdate = startdate, enddate = enddate, marine_only = marine_only, offset = offset, server = server, .opts = opts, ...),
    getAphiaRecordByID = fxn(AphiaID = id, server = server, .opts = opts, ...),
    getAphiaRecordByExtID = fxn(id = extid, type = type, server = server, .opts = opts, ...)
  )
  do.call(rbind, lapply(res, function(y) data.frame(unclass(y), stringsAsFactors = FALSE)))
}

#' Get external ID from Worms ID
#' @export
#' @template worms_id
#' @param type External ID source to get ID for. One of ncbi (default), tsn, bold, eol,
#' dyntaxa, fishbase, iucn or lsid.
#' @return Character class with ID and attributess for found or not, the source name, and a URL for
#' the taxon with the external source. No URL is given for lsid.
#' @examples \dontrun{
#' worms_extid(ids=1080, type='ncbi')
#' worms_extid(ids=278241, type='tsn')
#' worms_extid(ids=278241, type='iucn')
#' worms_extid(ids=282981, type='fishbase')
#' worms_extid(ids=127186, type='bold')
#' worms_extid(ids=278241, type='eol')
#' worms_extid(ids=278241, type='lsid')
#' worms_extid(ids=127186, type='dyntaxa')
#' }
worms_extid <- function(ids=NULL, type='ncbi', opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  upiface <- worms_update_iface(update_iface)
  if(!is.null(upiface)) worms_iface <- iface
  fxn <- worms_get_fxn('getExtIDbyAphiaID')
  res <- fxn(AphiaID = ids, type = type, server = server, .opts = opts)
  res <- if(length(res)==0) NA else res
  clazz <- switch(type, ncbi='uid', tsn='tsn', bold='bold', eol='eol', dyntaxa='dyntaxa', fishbase='fishbase', iucn='iucn', lsid='lsid')
  attr(res, "match") <- if(length(res)==0) 'not found' else 'found'
  attr(res, "uri") <- get_uri(type, res)
  class(res) <- clazz
  return(res)
}


##### helpers

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