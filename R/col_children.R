#' Search Catalogue of Life for for direct children of a particular taxon.
#'
#' @export
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
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @details You must provide one of name or id. The other parameters (format
#' 		and start) are optional.
#' @return A list of data.frame's.
#' @examples \dontrun{
#' # A basic example
#' col_children(name="Apis")
#'
#' # An example where there is no classification, results in data.frame with no rows
#' col_children(id='b2f88f382aa5568f93a97472c6be6516')
#'
#' # Use a specific year's checklist
#' col_children(name="Apis", checklist=2012)
#' col_children(name="Apis", checklist=2009)
#'
#' # Pass in many names or many id's
#' out <- col_children(name=c("Buteo","Apis","Accipiter","asdf"), checklist="2012")
#' out$Apis # get just the output you want
#' library("plyr")
#' ldply(out) # or combine to one data.frame
#'
#' # or pass many id's
#' ids <- c('abe977b1d27007a76dd12a5c93a637bf', 'b2f88f382aa5568f93a97472c6be6516')
#' out <- col_children(id = ids, checklist=2012)
#' library("plyr")
#' ldply(out) # combine to one data.frame
#' }
col_children <- function(name = NULL, id = NULL, format = NULL, start = NULL, checklist = NULL, ...) {
	if (is.null(id)) {
		temp <- llply(name, search_col_safe, id = NULL, checklist = checklist,
		              format = format, start = start, ...)
		setNames(temp, name)
	} else {
		temp <- llply(id, search_col_safe, name = NULL, checklist = checklist,
		              format = format, start = start, ...)
		setNames(temp, id)
	}
}

search_col <- function(name, id, checklist, format, start, ...) {
  url <- make_url(checklist)
  args <- tc(list(name = name, id = id, format = format, response = "full", start = start))
  out <- GET(col_base(), query = argsnull(args), ...)
  stop_for_status(out)
  tt <- xml2::read_xml(con_utf8(out))
  search_col_child_df(tt)
}
search_col_safe <- plyr::failwith(NULL, search_col)

search_col_child_df <- function(x) {
  childtaxa_id <- xml_text(xml_find_all(x, "//child_taxa//id"))
  childtaxa_name <- xml_text(xml_find_all(x, "//child_taxa//name"))
  childtaxa_rank <- xml_text(xml_find_all(x, "//child_taxa//rank"))
  data.frame(childtaxa_id, childtaxa_name, childtaxa_rank, stringsAsFactors = FALSE)
}
