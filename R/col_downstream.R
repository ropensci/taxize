#' Use Catalogue of Life to get downstream taxa to a given taxonomic level.
#' 
#' @import RCurl XML plyr
#' @param name The string to search for. Only exact matches found the name given 
#' 		will be returned, unless one or wildcards are included in the search string. 
#' 		An * (asterisk) character denotes a wildcard; a % (percentage) character 
#' 		may also be used. The name must be at least 3 characters long, not counting 
#' 		wildcard characters.
#' @param format format of the results returned. Valid values are format=xml and 
#' 		format=php; if the format parameter is omitted, the results are returned in 
#' 		the default XML format. If format=php then results are returned as a PHP 
#' 		array in serialized string format, which can be converted back to an array 
#' 		in PHP using the unserialize command
#' @param start The first record to return. If omitted, the results are returned 
#' 		from the first record (start=0). This is useful if the total number of 
#' 		results is larger than the maximum number of results returned by a single 
#' 		Web service query (currently the maximum number of results returned by a 
#' 		single query is 500 for terse queries and 50 for full queries).
#' @param checklist The year of the checklist to query, if you want a specific 
#' 		year's checklist instead of the lastest as default (numeric).
#' @param url The base COL url for the function (should be left to default).
#' @details You must provide one of name or id. The other parameters (format 
#' 		and start) are optional.
#' @return A list of data.frame's.
#' @examples \dontrun{
#' # Some basic examples
#' col_downstream(name="Apis", downto="Species")
#' col_downstream(name="Bryophyta", downto="Family")
#' 
#' # An example that takes a bit longer
#' col_downstream(name=c("Plantae","Animalia"), downto="Class")
#' }
#' @export
col_downstream <- function(name = NULL, downto, format = NULL, start = NULL, 
	checklist = NULL, url = "http://www.catalogueoflife.org/col/webservice")
{
	func <- function(name) {
		if(is.null(checklist)){NULL} else {
			cc <- match.arg(checklist, choices=c(2012,2011,2010,2009,2008,2007))
			if(cc %in% c(2012,2011,2010)){
				url <- gsub("col", paste("annual-checklist/", cc, sep=""), url)
			} else
			{
				url <- "http://webservice.catalogueoflife.org/annual-checklist/year/search.php"
				url <- gsub("year", cc, url)
			}
		}
		
		torank <- sapply(rank_ref[grep(downto, rank_ref$ranks):nrow(rank_ref),"ranks"], function(x) strsplit(x, ",")[[1]][[1]], USE.NAMES=F)
		
		toget <- name
		stop_ <- "not" 
		notout <- data.frame(rankName = "")
		out <- list()
		iter <- 0
		while(stop_ == "not"){
			iter <- iter + 1
			
			searchcol <- function(x) {
				args <- compact(list(name=x, format=format, response="full", start=start))
				out_ <- getForm(url, .params = args)
				tt <- xmlParse(out_)
				
				childtaxa_id <- xpathSApply(tt, "//child_taxa//id", xmlValue)
				childtaxa_name <- xpathSApply(tt, "//child_taxa//name", xmlValue)
				childtaxa_rank <- xpathSApply(tt, "//child_taxa//rank", xmlValue)
				data.frame(childtaxa_id, childtaxa_name, childtaxa_rank)
			}
			tt <- ldply(toget, searchcol)
		
			if(nrow(tt[tt$childtaxa_rank == downto, ]) > 0) out[[iter]] <- tt[tt$childtaxa_rank == downto, ]
			if(nrow(tt[!tt$childtaxa_rank == downto, ]) > 0) {
				notout <- tt[!tt$childtaxa_rank %in% torank, ]
			} else
				{ notout <- data.frame(rankName = downto) }
			
			if(all(notout$childtaxa_rank == downto)) { 
				stop_ <- "fam"
			} else
			{ 
				toget <- as.character(notout$childtaxa_name)
				stop_ <- "not" 
			}
			
		} # end while loop
		return(compact(out)[[1]])
		
	} # end fxn func
	safe_func <- plyr::failwith(NULL, func)
	temp <- llply(name, safe_func)
	names(temp) <- name
	temp
}