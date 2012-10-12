#' Search Taxonomic Name Resolution Service
#'
#' Match taxonomic names using the Taxonomic Name Resolution Service (TNRS). 
#'  Returns score of the matched name, and whether it was accepted or not.
#' @import RCurl XML plyr stringr RJSONIO
#' @param retrieve either 'best' or 'all' for returning the best matched or 
#'     all names, respectively (character).
#' @param taxnames quoted taxonomic names to search in a vector (character).
#' @param output 'all' for raw list output or 'names' for matched names
#'     and their match scores, plus plant family names (character).
#' @param getpost Use get or post for sending query. Post is sometimes needed 
#' 		for larger URL strings.
#' @param url The iPlant API url for the function (should be left to default).
#' @param ... optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass
#' the returned value in here (avoids unnecessary footprint) 
#' @return data.frame of results from TNRS plus the name submitted.
#' @export
#' @examples \dontrun{
#' mynames <- c("shorea robusta", "pandanus patina", "oryza sativa", "durio zibethinus", "rubus ulmifolius", "asclepias curassavica", "pistacia lentiscus")
#' iplant_tnrsmatch('best', mynames, 'names', 'post')
#' iplant_tnrsmatch(retrieve = 'all', taxnames = c('helianthus annuus', 'acacia', 'gossypium'), output = 'names')
#' iplant_tnrsmatch(retrieve = 'all', taxnames = c('helianthus annuus', 'acacia', 'saltea'), output = 'all')
#' iplant_tnrsmatch(retrieve = 'best', taxnames = c('helianthus annuus', 'acacia', 'saltea'), output = 'names')
#' }
iplant_tnrsmatch <- function(retrieve = 'best', taxnames = NA, output = NA, 
	getpost = 'get', url = "http://tnrs.iplantc.org/tnrsm-svc/matchNames",
  ..., curl = getCurlHandle()) 
{
  args <- list()
  if(!is.na(retrieve))
    args$retrieve <- retrieve
  if(!any(is.na(taxnames)))
    args$names <- paste(str_replace_all(taxnames, ' ', '%20'), collapse=',')
  message("Hitting the TNRS API and matching names...")
  if(getpost == 'get'){tt<-getForm(url, .params = args, ..., curl = curl)} else
  	if(getpost == 'post'){tt<-postForm(url, .params = args, ..., curl = curl, style="post")} else
  		stop("use either 'get' or 'post'")
  out <- fromJSON(tt)
  if (output == 'all') { return(out) } else
    if (output == 'names') {
        outdf <- llply(out[[1]], function(y) c(y$group, y$acceptedName, y$family, 
            y$genus, round(as.numeric(y$overall), 2), y$acceptance))
        outdf <- ldply(outdf)
        names(outdf) <- c('Group', 'AcceptedName','MatchFam','MatchGenus','MatchScore','Accept?')
        namesgroups <- data.frame(Group = (1:length(taxnames))-1, SubmittedNames = taxnames)
        merge(outdf, namesgroups, by='Group')[,-1]
      } else
      	stop("use with 'all' or 'names'")
}