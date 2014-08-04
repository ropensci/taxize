#' Synonyms search
#' 
#' @export
#' @template worms_id
#' @examples \dontrun{
#' worms_synonyms(ids=733271)
#' worms_synonyms(ids=c(733271,125725,159283))
#' }
worms_synonyms <- function(ids=NULL, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  if(!is.null(iface)) worms_iface <- iface
  fxn <- worms_get_fxn('getAphiaSynonymsByID')
  res <- lapply(ids, fxn, server = server, .opts = opts)
  do.call(rbind, lapply(res, function(y) if(length(y)==1){
      data.frame(inputid=y[[1]]$AphiaID, unclass(y[[1]]), stringsAsFactors = FALSE)
    } else {
      do.call(rbind, lapply(y, function(z) data.frame(inputid=y[[1]]$AphiaID, unclass(z), stringsAsFactors = FALSE)))
    }
  ))
}
