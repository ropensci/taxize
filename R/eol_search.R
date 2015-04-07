#' Search for terms in EOL database.
#'
#' @export
#' @param terms search terms (character)
#' @param page A maximum of 30 results are returned per page. This parameter allows
#'    you to fetch more pages of results if there are more than 30 matches (Default 1)
#' @param exact Will find taxon pages if the preferred name or any synonym or common
#'    name exactly matches the search term.
#' @param filter_tid Given an EOL page ID, search results will be limited to members
#'    of that taxonomic group
#' @param filter_heid Given a Hierarchy Entry ID, search results will be limited to
#'    members of that taxonomic group
#' @param filter_by_string Given a search term, an exact search will be made and that
#'    matching page will be used as the taxonomic group against which to filter search
#'    results
#' @param cache_ttl The number of seconds you wish to have the response cached.
#' @param key Your EOL API key; loads from .Rprofile.
#' @param callopts Curl options passed on to getForm.
#' @details It's possible to return JSON or XML with the EOL API. However,
#' 		this function only returns JSON for now.
#' @return A data frame.
#' @examples \dontrun{
#' eol_search(terms='Homo')
#' eol_search(terms='Salix')
#' eol_search(terms='Ursus americanus luteolus')
#' }

eol_search <- function(terms, page=1, exact=NULL, filter_tid=NULL, filter_heid=NULL,
  filter_by_string=NULL, cache_ttl=NULL, key = NULL, ...) {

	key <- getkey(key, "eolApiKey")
	query <- gsub("\\s", "+", terms)
  args <- taxize_compact(list(q=query,page=page,exact=exact,filter_by_taxon_concept_id=filter_tid,
      filter_by_hierarchy_entry_id=filter_heid, filter_by_string=filter_by_string,
      cache_ttl=cache_ttl, key = key))
  tt <- GET(paste0(eol_url("search"), ".json"), query = args, ...)
  warn_for_status(tt)
  stopifnot(tt$headers$`content-type`[1] == 'application/json; charset=utf-8')
  parsed <- content(tt, as = "text")
  res <- jsonlite::fromJSON(parsed, FALSE, encoding = "utf-8")
  if (res$totalResults == 0 | length(res$results) == 0) {
    data.frame(pageid = NA, name = NA, stringsAsFactors = FALSE)
  } else {
    tmp <- do.call(rbind.fill, lapply(res$results, data.frame, stringsAsFactors=FALSE))[,c('id','title')]
    setNames(tmp, c("pageid", "name"))
  }
}
