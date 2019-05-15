#' @title Search for taxonomic names using the Global Names Index
#'
#' @description Uses the Global Names Index, see http://gni.globalnames.org
#'
#' @export
#' @param search_term Name pattern you want to search for. WARNING: Does
#' not work for vernacular/common names. Search term may include following
#' options (Note: can, uni, gen, sp, ssp, au, yr work only for parsed names
#'
#' * *	wild card - Search by part of a word (E.g.: planta*)
#' * exact exact match	- Search for exact match of a literal string
#'  (E.g.: exact:Parus major)
#' * ns name string- Search for literal string from its beginning (other
#'  modifiers will be ignored) (E.g.: ns:parus maj*)
#' * can canonical form- Search name without authors (other modifiers will
#'  be ignored)	(E.g.: can:parus major)
#' * uni uninomial- Search for higher taxa	(E.g.: uni:parus)
#' * gen genus - Search by genus epithet of species name (E.g.: gen:parus)
#' * sp species - Search by species epithet (E.g.: sp:major)
#' * ssp subspecies - Search by infraspecies epithet (E.g.: ssp:major)
#' * au author - Search by author word	(E.g.: au:Shipunov)
#' * yr year - Search by year (E.g.: yr:2005)
#'
#' @param per_page Number of items per one page (numbers larger
#' 		than 1000 will be decreased to 1000) (default is 30).
#' @param page Page number you want to see (default is 1).
#' @param justtotal Return only the total results found.
#' @param parse_names If `TRUE`, use [gni_parse()] to parse
#' names. Default: `FALSE`
#' @param ... Curl options passed on to [crul::verb-GET]
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @return data.frame of results.
#' @seealso [gnr_datasources()], [gni_search()]
#' @keywords globalnamesindex names taxonomy
#' @references <http://gni.globalnames.org/>,
#' <https://github.com/dimus/gni/wiki/api>
#' @details Note that you can use fuzzy searching, e.g., by attaching an
#' asterisk to the end of a search term. See the first two examples below
#' @examples \dontrun{
#' gni_search(search_term = "ani*")
#' gni_search(search_term = "ama*", per_page = 3, page = 21)
#' gni_search(search_term = "animalia", per_page = 8, page = 1)
#' gni_search(search_term = "animalia", per_page = 8, page = 1, justtotal=TRUE)
#'
#' gni_search(search_term = "Cyanistes caeruleus", parse_names=TRUE)
#'
#' # pass on curl options
#' gni_search(search_term = "ani*", verbose = TRUE)
#' }
gni_search <- function(search_term = NULL, per_page = NULL, page = NULL,
  justtotal = FALSE, parse_names = FALSE, ...) {

	query <- tc(list(search_term = search_term, per_page = per_page,
        page = page))
  cli <- crul::HttpClient$new(paste0(gni_base(), "name_strings.json"),
    headers = tx_ual, opts = list(...))
  tt <- cli$get(query = argsnull(query))
  tt$raise_for_status()
	out <- jsonlite::fromJSON(tt$parse("UTF-8"), FALSE)

	if (justtotal) {
	  out$name_strings_total
	} else {
	  df <- ldply(out$name_strings, function(x)
	    t(data.frame(c( checknull(x[["name"]]), checknull(x[["id"]]),
	                    checknull(x[["lsid"]]), checknull(x[["uuid_hex"]]),
	                    checknull(x[["resource_url"]]) ))))
	  df <- colClasses(df, "character")
	  if (NROW(df) != 0) {
	    names(df) <- c("name","id","lsid","uuid_hex","resource_url")
	  }

    if (parse_names) {
      data.frame(df, gni_parse(as.character(df$name)),
        stringsAsFactors = FALSE)
    } else {
      df
    }
	}
}
