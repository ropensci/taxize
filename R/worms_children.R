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
  res <- lapply(ids, fxn, offset = offset, marine_only = marine_only, server = server, .opts = opts) 
  parse_data(res)
}

# #' @method print worms
# #' @export
# #' @rdname worms
# print.worms <- function(x, ...){
#   if(!is(x, "worms")) stop('Input must be of class worms')
#   print(x[ , !names(x) %in% c('url','citation') ])
# }