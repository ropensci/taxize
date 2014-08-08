#' Children search of WoRMS data.
#' 
#' @export
#' @template worms_id
#' @param offset Starting record number, when retrieving next chunk of (50) records. Default=1.
#' @param marine_only (logical) Include results from marine taxa only. Default: TRUE.
#' @examples \dontrun{
#' worms_children(ids=106135)
#' worms_children(ids=c(106135,159283))
#' out <- worms_children(ids=c(106135,159283))
#' head(out)
#' }
worms_children <- function(ids=NULL, offset=NULL, marine_only=1, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  if(!is.null(iface)) worms_iface <- iface
  fxn <- worms_get_fxn('getAphiaChildrenByID')
  res <- lapply(ids, fxn, offset = offset, marine_only = marine_only, server = server, .opts = opts, .convert=FALSE) 
#   parse_data(res)
  do.call(rbind, Map(worms_parse_xml, res, aphiaid=ids))
}

worms_parse_xml <- function(z, aphiaid, which="getAphiaChildrenByID")
{
  which <- if(which %in% c('getAphiaChildrenByID','getAphiaRecords','getAphiaRecordsByNames','getAphiaRecordsByVernacular','getAphiaRecordsByDate')) '//item' else '//return'
  st <- xmlParse( z$content )
  ns <- c(xmlns='xsi="http://www.w3.org/2001/XMLSchema-instance"')
  nodes <- getNodeSet(st, which, namespaces = ns)
  out <- lapply(nodes, function(x){
    if(length(getNodeSet(x, "item")) == 0){
      extract_it(x)
    } else {
      tmp <- getNodeSet(x, 'item')
      do.call(rbind.fill, lapply(tmp, extract_it))
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
