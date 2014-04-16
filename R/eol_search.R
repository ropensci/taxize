#' Search for terms in EOL database.
#' 
#' @import RCurl plyr assertthat RJSONIO
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
#' @return JSON list object, or data.frame.
#' @examples \dontrun{
#' eol_search('Homo')
#' eol_search(terms='Salix')
#' eol_search(terms='Ursus americanus luteolus')
#' }

eol_search <- function(terms, page=1, exact=NULL, filter_tid=NULL, filter_heid=NULL,
  filter_by_string=NULL, cache_ttl=NULL, key = NULL, callopts=list()) 
{     
  url = 'http://eol.org/api/search/1.0.json'
	key <- getkey(key, "eolApiKey")
	query <- gsub("\\s", "+", terms)
  args <- compact(list(q=query,page=page,exact=exact,filter_by_taxon_concept_id=filter_tid,
                       filter_by_hierarchy_entry_id=filter_heid, 
                       filter_by_string=filter_by_string,cache_ttl=cache_ttl))
#   callopts <- c(add_headers(`Content-type` = "application/json; charset=utf-8"), callopts)
#   tt <- GET(url, query=args, callopts)
#   stop_for_status(tt)
  tt <- getForm(url, .params=args, .opts = callopts)
  assert_that(attributes(tt)$`Content-Type`[1]=='application/json')
  assert_that(attributes(tt)$`Content-Type`[[2]]=='utf-8')
#   temp <- content(tt, as = "text", encoding = "utf-8")
  res <- RJSONIO::fromJSON(tt, simplifyWithNames = FALSE, encoding = "utf-8")
  if(res$totalResults == 0){
    data.frame(pageid=NA, name=NA, stringsAsFactors = FALSE)
  } else
  { 
    tmp <- do.call(rbind.fill, lapply(res$results, data.frame, stringsAsFactors=FALSE))[,c('id','title')]
    names(tmp) <- c("pageid","name")
    tmp
  }
}