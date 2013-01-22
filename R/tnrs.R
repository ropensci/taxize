#' Search the Phylotastic Taxonomic Name Resolution Service.
#'
#' Match taxonomic names using the Taxonomic Name Resolution Service (TNRS). 
#'  Returns score of the matched name, and whether it was accepted or not.
#'  
#' @import RCurl XML plyr stringr RJSONIO
#' @param query Quoted taxonomic names to search in a vector (character).
#' @param shortenurls Use bit.ly API to shorten URLs for matched species (default = FALSE).
#' @param getpost Use GET or POST method to send the query. If you have more than 
#' 		say 50 species or so in your query, you should probably use POST.
#' @param sleep Numer of seconds by which to pause by before retrieving the 
#'    result. Defaults to 1 second. Set sleep for longer periods when queries
#'    contain more species.
#' @param url The iPlant API url for the function (should be left to default).
#' @return data.frame of results from TNRS plus the name submitted.
#' @export
#' @examples \dontrun{
#' # Default, uses GET curl method
#' mynames <- c("Panthera tigris", "Eutamias minimus", "Magnifera indica", "Humbert humbert")
#' tnrs(query = mynames)
#' 
#' # Using POST method, especially useful when you have a lot of species
#' mynames <- c("Panthera tigris", "Eutamias minimus", "Magnifera indica", "Humbert humbert", 
#' 		"Helianthus annuus", "Pinus contorta", "Poa annua", "Abies magnifica", 
#'		"Rosa california", "Festuca arundinace", "Mimulus bicolor", "Sorbus occidentalis",
#'		"Madia sativa", "Thymopsis thymodes", "Bartlettia scaposa")
#' tnrs(mynames, getpost="POST")
#' }
tnrs <- function(query = NA, shortenurls = FALSE, getpost = "GET", sleep = 1,
  url = "http://api.phylotastic.org/tnrs/submit")
{
  if(sleep==1){sleep <- sleep * length(query)} else # sleep=1 times the number of species
  	{sleep <- sleep}
  
  if(getpost=="GET"){
  	if(!any(is.na(query)))
  		query2 <- paste(str_replace_all(query, ' ', '+'), collapse='%0A')
  	tt <- getURL(paste0(url, "?query=", query2))
  } else
  {
  	splist <- paste(mynames, collapse="\n")
  	tt <- postForm(url, query=splist)
  }
  message <- fromJSON(tt)["message"]
  retrieve <- str_replace_all(str_extract(message, "http.+"), "\\.$", "")
  token <- str_split(retrieve, "/")[[1]][[6]]
  
  message(paste("Token number ", token, sep=""))
  message(paste("Pausing ", sleep, " seconds for the query to finish...", sep=""))

  Sys.sleep(time = sleep) # set amount of sleep to pause by
  out <- fromJSON(getURL(retrieve)) # retrieve data
  
  foo <- function(x, shortenurls=FALSE){ # function to parse results
  	matches <- x$matches
  	foome <- function(x) { 
  		x[sapply(x, length)==0] <- "none" 
  		data.frame(x)
  	}
  	matches2 <- ldply(matches, foome)
  	df <- data.frame(submittedName=x$submittedName, matches2)
  	df$score <- round(as.numeric(as.character(df$score)), 2)
  	if(shortenurls) df$uri <- llply(df$uri, bitly_shorten)
  	df
  }
  
  df <- ldply(out$names, foo, shortenurls) 
  return(df)
}