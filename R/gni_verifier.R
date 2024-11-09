#' @title Verify scientific names with GNI
#'
#' @description "Verify a list of scientific names against biodiversity
#'   data-sources. This service parses incoming names, executes exact or fuzzy
#'   matching as required, and returns the best-scored result. Optionally, it
#'   can also return matches from data-sources selected by a user."
#'   -http://gni.globalnames.org/
#' 
#' @export
#'
















#' @title Search for taxonomic names using the Global Names Index
#'
#' @description Uses the Global Names Index, see http://gni.globalnames.org
#'
#' @export
#' @param sci (character) required. Name pattern you want to search
#' for. WARNING: Does not work for common names. Search term may include
#' following options:
#'
#' * `n`: A shortcut that allows to put together several elements (e.g., `n:B. bubo Linn. 1750-1800`)
#' * `g`: a genus name. (e.g. `g:B.`, `g:Bub.`, `g:Bubo`)
#' * `isp`: an infraspecies name (e.g. `sp:bubo`, `sp:gallop.`)
#' * `asp`: either species or infraspecies (all sp) (e.g. `asp:bubo`)
#' * `ds`: data-sources IDs (e.g., `ds:1,2,3`)
#' * `tx`: parent taxon . Uses classification of the first data-source from `ds`. If data-sources are not set, uses Catalogue of Life. (e.g. `tx:Aves`)
#' * `au`: author - Search by author word	(g.g. `au:Linnaeus`, `au:Linn.`)
#' * `y`: year - Search by year (e.g. `y:2005`)
#'
#' @param justtotal Return only the total results found.
#' @param parse_names If `TRUE`, use [gni_parse()] to parse
#' @param per_page Number of items per one page (numbers larger
#' than 1000 will be decreased to 1000) (default is 30).
#' @param page Page number you want to see (default is 1).
#' names. Default: `FALSE`
#' @param search_term Deprecated, see `sci`
#' @param ... Curl options passed on to [crul::verb-GET]
#' @author Scott Chamberlain, Zachary Foster
#' @return data.frame of results.
#' @seealso [gnr_datasources()], [gni_search()]
#' @keywords globalnamesindex names taxonomy
#' @references http://gni.globalnames.org/
#' https://apidoc.globalnames.org/gnames
#' 
#' @examples \dontrun{
#' gni_search('n:B. bubo ds:1,2 au:Linn. y:1700-')
#' }
gni_search <- function(sci, justtotal = FALSE, parse_names = FALSE, 
                       per_page = NULL, page = NULL, search_term = NULL, ...) {
  
  if (!is.null(search_term)) {
    lifecycle::deprecate_warn(when = "v0.9.97", what = "gni_search(search_term)", with = "gni_search(sci)")
    sci <- search_term
  }
  if (!is.null(per_page)) {
    lifecycle::deprecate_warn(when = "v0.9.97", what = "gni_search(per_page)")
  }
  if (!is.null(page)) {
    lifecycle::deprecate_warn(when = "v0.9.97", what = "gni_search(page)")
  }
  
  query <- tc(list(search_term = sci))
  cli <- crul::HttpClient$new('https://verifier.globalnames.org',
                               headers = tx_ual, opts = list(...))
  tt <- cli$get(path = paste0('/api/v1/search/', curl::curl_escape(sci)))
  tt$raise_for_status()
  out <- jsonlite::fromJSON(tt$parse("UTF-8"), FALSE)
  
  if (justtotal) {
    return(out$metadata$namesNumber)
  } else {
    df <- do.call(rbind, lapply(out$names, function(x) {
      all_feilds <- unlist(x)
      names(all_feilds) <- vapply(strsplit(names(all_feilds), split = '.', fixed = TRUE), function(x) x[length(x)], FUN.VALUE = character(1))
      all_feilds <- all_feilds[! duplicated(names(all_feilds))]
      return(as.data.frame(as.list(all_feilds)))
    }))
    df <- colClasses(df, "character")
    df <- tibble::as_tibble(df)
    if (NROW(df) != 0) {
      names(df) <- c("id", "name", "cardinality", "matchType", "dataSourceId", "dataSourceTitleShort", 
                     "curation", "recordId", "outlink", "entryDate", "sortScore", 
                     "matchedNameID", "matchedName", "matchedCardinality", "currentRecordId", 
                     "currentNameId", "currentName", "currentCardinality", "currentCanonicalSimple", 
                     "currentCanonicalFull", "taxonomicStatus", "isSynonym", "classificationPath", 
                     "classificationRanks", "classificationIds", "editDistance", "stemEditDistance", 
                     "cardinalityScore", "infraSpecificRankScore", "fuzzyLessScore", 
                     "curatedDataScore", "authorMatchScore", "acceptedNameScore", 
                     "parsingQualityScore", "dataSourcesNum", "dataSourcesIds")
    }
    
    if (parse_names) {
      data.frame(df, gni_parse(as.character(df$name)), stringsAsFactors = FALSE)
    } else {
      df
    }
  }
}
