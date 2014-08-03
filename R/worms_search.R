#' Search World Register of Marine Species (WoRMS)
#' 
#' @export
#' @import SSOAP
#' @param names asdadsf
#' @param by asdfda
#' @param update_iface ssdafdf
#' @details You must provide one of name or id. The other parameters (format 
#' 		and start) are optional. \code{getAphiaRecordByExtID} is not available through this fxn.
#'   	Search by AphiaID, scientific name, vernacular name, or date.
#' @references \url{http://www.marinespecies.org/}
#' @return A data.frame.
#' @examples \dontrun{
#' # A basic example
#' worms_search(names="Apis")
#' worms_search(names='', by='getAphiaRecordsByNames')
#' }

worms_search_name <- function(names=NULL, by='getAphiaID', update_iface=FALSE)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  upiface <- worms_update_iface(update_iface)
  if(!is.null(upiface)) worms_iface <- iface
  fxn_name <- match.arg(by, c('getAphiaID','getAphiaRecords','getAphiaNameByID','getAphiaRecordByID',
                  'getExtIDbyAphiaID','getAphiaRecordsByNames','getAphiaRecordsByVernacular',
                  'getAphiaRecordsByDate','getAphiaClassificationByID',
                  'getSourcesByAphiaID','getAphiaSynonymsByID','getAphiaVernacularsByID',
                  'getAphiaChildrenByID','matchAphiaRecordsByNames'))
  fxn <- worms_get_fxn(fxn_name)
  fxn(scientificname = "Solea", 0, server = server)
}

worms_search_id <- function(ids=NULL, offset=NULL, marine_only=TRUE, by='getAphiaNameByID', update_iface=FALSE)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  upiface <- worms_update_iface(update_iface)
  if(!is.null(upiface)) worms_iface <- iface
  fxn_name <- match.arg(by, c('getAphiaNameByID','getAphiaRecordByID','getExtIDbyAphiaID',
                  'getAphiaClassificationByID','getSourcesByAphiaID','getAphiaSynonymsByID',
                  'getAphiaVernacularsByID','getAphiaChildrenByID'))
  fxn <- worms_get_fxn(fxn_name)
  fxn(scientificname = "Solea", 0, server = server)
}

#' Hierarchy search
#' 
#' @examples \dontrun{
#' worms_hierarchy(ids=733271)
#' }
worms_hierarchy <- function(ids=NULL, opts=NULL, update_iface=FALSE)
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
#' 
#' @examples \dontrun{
#' worms_children(ids=106135)
#' }
worms_children <- function(ids=NULL, offset=NULL, marine_only=1, opts=NULL, update_iface=FALSE)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  upiface <- worms_update_iface(update_iface)
  if(!is.null(upiface)) worms_iface <- iface
  fxn <- worms_get_fxn('getAphiaChildrenByID')
  res <- fxn(AphiaID = ids, offset = offset, marine_only = marine_only, server = server, .opts = opts)
  do.call(rbind, lapply(res, function(y) data.frame(unclass(y), stringsAsFactors = FALSE)))
}

#' Synonyms search
#' 
#' @examples \dontrun{
#' worms_synonyms(ids=733271)
#' }
worms_synonyms <- function(ids=NULL, opts=NULL, update_iface=FALSE)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  upiface <- worms_update_iface(update_iface)
  if(!is.null(upiface)) worms_iface <- iface
  fxn <- worms_get_fxn('getAphiaSynonymsByID')
  res <- fxn(AphiaID = ids, server = server, .opts = opts)
  do.call(rbind, lapply(res, function(y) data.frame(unclass(y), stringsAsFactors = FALSE)))
}

#' Common names from ID
#' 
#' @template worms_id
#' @examples \dontrun{
#' worms_common(ids=1080)
#' worms_common(ids=22388)
#' worms_common(ids=123080)
#' worms_common(ids=160281)
#' }
worms_common <- function(ids=NULL, opts=NULL, update_iface=FALSE, ...)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  upiface <- worms_update_iface(update_iface)
  if(!is.null(upiface)) worms_iface <- iface
  fxn <- worms_get_fxn('getAphiaVernacularsByID')
  res <- fxn(AphiaID = ids, server = server, .opts = opts)
  do.call(rbind, lapply(res, function(y) data.frame(unclass(y), stringsAsFactors = FALSE)))
}

#' Get external ID from Worms ID
#' 
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
worms_extid <- function(ids=NULL, type='ncbi', opts=NULL, update_iface=FALSE, ...)
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
worms_update_iface <- function(update_iface=FALSE, wsdl_url="http://www.marinespecies.org/aphia.php?p=soap&wsdl=1")
{
  if(update_iface){
    w <- processWSDL(wsdl_url)
    genSOAPClientInterface(, w)
  } else { NULL }
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