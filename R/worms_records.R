#' Get records by ID, scientific name, common name, date, worms id, or external id.
#' 
#' @export
#' @template worms_id
#' @param scientific (character) A scientific name.
#' @param common (character) A common name.
#' @param id (integer) A WoRMS (or Aphia) identifier.
#' @param extid (integer) External identifier.
#' @param like Add a percent sign added after the ScientificName (SQL LIKE function). Default=TRUE
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