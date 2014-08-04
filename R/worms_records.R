#' Get records by ID, scientific name, common name, date, worms id, or external id.
#'
#' @export
#' @template worms_id
#' @param scientific (character) A scientific name.
#' @param common (character) A common name.
#' @param extids (integer) External identifier.
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
#' worms_records(scientific=c('Salmo','Aphanius'))
#' worms_records(scientific='Liopsetta glacialis')
#' worms_records(common='salmon')
#' worms_records(common=c('salmon','char'))
#' worms_records(startdate='2014-06-01T00:00:00', enddate='2014-06-02T00:00:00')
#' worms_records(ids=1080)
#' worms_records(extids=6830, type='ncbi')
#' }
worms_records <- function(scientific=NULL, common=NULL, ids=NULL, extids=NULL, like=NULL, type=NULL,
  marine_only=1, offset=NULL, startdate=NULL, enddate=NULL, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  if(!is.null(iface)) worms_iface <- iface
  endpt <- if(!is.null(common)){
    'getAphiaRecordsByVernacular'
  } else if (!is.null(startdate)|!is.null(enddate)) {
    'getAphiaRecordsByDate'
  } else if(!is.null(extids)) {
    'getAphiaRecordByExtID'
  } else if(!is.null(ids)){
    'getAphiaRecordByID'
  } else {
    if(length(scientific) > 1) 'getAphiaRecordsByNames' else 'getAphiaRecords'
  }
  fxn <- worms_get_fxn(endpt)
  res <- switch(endpt,
    getAphiaRecords = lapply(scientific, fxn, like = like, fuzzy = 'false', marine_only = marine_only, offset = 'false', server = server, .opts = opts, ...),
    getAphiaRecordsByNames = lapply(scientific, fxn, like = like, fuzzy = 'false', marine_only = marine_only, server = server, .opts = opts, ...),
    getAphiaRecordsByVernacular = lapply(common, fxn, like = like, offset = offset, server = server, .opts = opts, ...),
    getAphiaRecordsByDate = lapply(startdate, fxn, enddate = enddate, marine_only = marine_only, offset = offset, server = server, .opts = opts, ...),
    getAphiaRecordByID = lapply(ids, fxn, server = server, .opts = opts, ...),
    getAphiaRecordByExtID = lapply(extids, fxn, type = type, server = server, .opts = opts, ...)
  )
  names(res) <- switch(endpt, getAphiaRecords=scientific, getAphiaRecordsByNames=scientific, getAphiaRecordsByVernacular=common, getAphiaRecordsByDate=startdate, getAphiaRecordByID=ids, getAphiaRecordByExtID=extids)
  parse_data_byname(res)
}
