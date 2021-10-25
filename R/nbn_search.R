#' Search UK National Biodiversity Network
#'
#' @export
#' @param sci_com (character) The query terms(s), a scientific or common name
#' @param fq (character) Filters to be applied to the original query. These
#' are additional params of the form fq=INDEXEDFIELD:VALUE e.g.
#' fq=rank:kingdom. See https://species-ws.nbnatlas.org/indexFields for all
#' the fields that are queryable.
#' @param rows (integer) Number of records to return
#' @param start (integer) Record offset, to enable paging
#' @param sort (character) The indexed field to sort by
#' @param order (character) Supports "asc" or "desc"
#' @param facets (list) Comma separated list of the fields to create facets
#' on e.g. facets=basis_of_record.
#' @param q Deprecated, see `sci`
#' @param ... Further args passed on to [crul::HttpClient].
#' @family nbn
#' @return a list with slots for metadata (`meta`) with list of response
#' attributes, and data (`data`) with a [tibble::tibble] of results
#' @author Scott Chamberlain, 
#' @references https://api.nbnatlas.org/
#'
#' @examples \dontrun{
#' x <- nbn_search(sci_com = "Vulpes")
#' x$meta$totalRecords
#' x$meta$pageSize
#' x$meta$urlParameters
#' x$meta$queryTitle
#' head(x$data)
#'
#' nbn_search(sci_com = "blackbird", start = 4)
#'
#' # debug curl stuff
#' nbn_search(sci_com = "blackbird", verbose = TRUE)
#' }
nbn_search <- function(sci_com, fq = NULL, order = NULL, sort = NULL,
  start = 0, rows = 25, facets = NULL, q = NULL, ...) {

  pchk(q, "sci_com")
  args <- tc(list(
    q = sci_com, fq = fq, pageSize = rows, startIndex = start, sort = sort,
    dir = order, facets = facets
  ))
  nested_list_df_to_tibbles(nbn_GET(file.path(nbn_base(), "search"), args, ...))
}

nbn_GET <- function(url, args, ...){
  cli <- crul::HttpClient$new(url = url, headers = tx_ual,)
  tt <- cli$get(query = argsnull(args), ...)
  tt$raise_for_status()
  json <- jsonlite::fromJSON(tt$parse("UTF-8"))$searchResults
  list(meta = pop(json, "results"), data = json$results)
}

nbn_base <- function() "https://species-ws.nbnatlas.org"
