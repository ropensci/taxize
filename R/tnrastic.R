#' Search the Phylotastic Taxonomic Name Resolution Service
#'
#' Match taxonomic names using the Taxonomic Name Resolution Service (TNRS). 
#'  Returns score of the matched name, and whether it was accepted or not.
#'  
#' @import RCurl XML plyr stringr RJSONIO
#' @param query Quoted taxonomic names to search in a vector (character).
#' @param output 'all' for raw list output or 'names' for matched names
#'     and their match scores, plus plant family names (character).
#' @param sleep Numer of seconds by which to pause by before retrieving the 
#'    result. Defaults to 1 second. Set sleep for longer periods when queries
#'    contain more species.
#' @param getpost Use get or post for sending query. Post is sometimes needed 
#' 		for larger URL strings.
#' @param url The iPlant API url for the function (should be left to default).
#' @param ... optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass
#' the returned value in here (avoids unnecessary footprint) 
#' @return data.frame of results from TNRS plus the name submitted.
#' @export
#' @examples \dontrun{
#' mynames <- c("Crepis atrabarba", "Zygadenus venenosus")
#' tnrastic(query = mynames, output = 'names')
#' }
tnrastic <- function(query = NA, output = NA, sleep = 1, getpost = 'get',
  url = "http://api.phylotastic.org/tnrs/submit",
  ..., curl = getCurlHandle()) 
{
  args <- list()
  if(!any(is.na(query)))
    args$query <- paste(str_replace_all(query, ' ', '+'), collapse='%0A')
  if(getpost == 'get'){tt<-getForm(url, .params = args, ..., curl = curl)} else
  	if(getpost == 'post'){tt<-postForm(url, .params = args, ..., curl = curl)} else
  		stop("use either 'get' or 'post'")
  out_ <- fromJSON(tt, simplify=F)
  message(out_$message)
  message("Pausing a bit for the query to finish...")
  Sys.sleep(time = sleep)
  out <- fromJSON(getURL(out_$uri))
  
  if (output == 'all') { return(out) } else
    if (output == 'names') {
      outdf <- ldply(out[[1]], function(y) c(y[[2]][[1]]$acceptedName, y[[2]][[1]]$sourceId, 
                            round(as.numeric(y[[2]][[1]]$score), 2), y[[3]][[1]]))
      names(outdf) <- c('AcceptedName','sourceId','MatchScore','submittedName')
      return(outdf)
    }      
}