#' Search Catalogue of Life for for direct children of a particular taxon.
#'
#' @import RCurl XML plyr
#' @param name The string to search for. Only exact matches found the name given
#' 		will be returned, unless one or wildcards are included in the search
#'   	string. An * (asterisk) character denotes a wildcard; a % (percentage)
#'    character may also be used. The name must be at least 3 characters long,
#'    not counting wildcard characters.
#' @param id The record ID of the specific record to return (only for scientific
#' 		names of species or infraspecific taxa)
#' @param format format of the results returned. Valid values are format=xml and
#' 		format=php; if the format parameter is omitted, the results are returned
#'   	in the default XML format. If format=php then results are returned as a
#'    PHP array in serialized string format, which can be converted back to an
#'    array in PHP using the unserialize command
#' @param start The first record to return. If omitted, the results are returned
#' 		from the first record (start=0). This is useful if the total number of
#' 		results is larger than the maximum number of results returned by a single
#' 		Web service query (currently the maximum number of results returned by a
#' 		single query is 500 for terse queries and 50 for full queries).
#' @param checklist The year of the checklist to query, if you want a specific
#' 		year's checklist instead of the lastest as default (numeric).
#' @details You must provide one of name or id. The other parameters (format
#' 		and start) are optional.
#' @return A list of data.frame's.
#' @examples \dontrun{
#' # A basic example
#' col_children(name="Apis")
#'
#' # An example where there is no classification, results in data.frame with no rows
#' col_children(id=11935941)
#'
#' # Use a specific year's checklist
#' col_children(name="Apis", checklist="2012")
#' col_children(name="Apis", checklist="2009")
#'
#' # Pass in many names or many id's
#' out <- col_children(name=c("Buteo","Apis","Accipiter","asdf"), checklist="2012")
#' out$Apis # get just the output you want
#' library(plyr)
#' ldply(out) # or combine to one data.frame
#'
#' # or pass many id's
#' out <- col_children(id=c(2346405,2344165,2346405), checklist="2012")
#' library(plyr)
#' ldply(out) # combine to one data.frame
#' }
#' @export

col_children <- function(name = NULL, id = NULL, format = NULL, start = NULL,
	checklist = NULL)
{
  url = "http://www.catalogueoflife.org/col/webservice"
	func <- function(x, y) {
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

		args <- compact(list(name=x, id=y, format=format, response="full", start=start))
		out <- getForm(url, .params = args)
		tt <- xmlParse(out)

		childtaxa_id <- xpathSApply(tt, "//child_taxa//id", xmlValue)
		childtaxa_name <- xpathSApply(tt, "//child_taxa//name", xmlValue)
		childtaxa_rank <- xpathSApply(tt, "//child_taxa//rank", xmlValue)
		data.frame(childtaxa_id, childtaxa_name, childtaxa_rank, stringsAsFactors = FALSE)
	}
	safe_func <- plyr::failwith(NULL, func)
	if(is.null(id)){
		temp <- llply(name, safe_func, y=NULL)
		names(temp) <- name
		temp
	} else {
		temp <- llply(id, safe_func, x=NULL)
		names(temp) <- id
		temp
	}
}
