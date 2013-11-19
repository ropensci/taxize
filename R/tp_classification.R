#' Return all synonyms for a taxon name with a given id.
#' 
#' @import httr plyr
#' @param id A Tropicos name ID
#' @param key Your Tropicos API key; loads from .Rprofile.
#' @param callopts Further args passed on to httr::GET
#' @return A data.frame giving the hierarchy.
#' @references \url{http://services.tropicos.org/help?method=GetNameHigherTaxaXml}
#' @export
#' @examples \dontrun{
#' tp_classification(id = 25509881)
#' tp_classification(id = c(25509881,2700851))
#' tp_classification(id = c(25509881,2700851), callopts=verbose())
#' }
tp_classification <- function(id=NULL, key=NULL, callopts=list())
{
  fun <- function(x){
    url <- sprintf('http://services.tropicos.org/Name/%s/HigherTaxa', x)
    key <- getkey(key, "tropicosApiKey")
    args <- compact(list(format='json', apikey=key))
    tt <- GET(url, query=args, callopts)
    stop_for_status(tt)
    out <- content(tt)
    if(names(out[[1]])[[1]] == "Error"){ data.frame(NameId=NA, ScientificName=NA, Rank=NA) } else {
      do.call(rbind.fill, lapply(out, data.frame))[,c('NameId','ScientificName','Rank')]      
    }
  }
  tmp <- lapply(id, fun)
  names(tmp) <- id
  tmp
}