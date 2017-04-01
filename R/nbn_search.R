#' Search UK National Biodiversity Network
#'
#' @export
#' @param q (character) The query terms(s)
#' @param fq (character) Filters to be applied to the original query. These
#' are additional params of the form fq=INDEXEDFIELD:VALUE e.g.
#' fq=rank:kingdom. See <https://species-ws.nbnatlas.org/indexFields> for all
#' the fields that are queryable.
#' @param rows (integer) Number of records to return
#' @param start (integer) Record offset, to enable paging
#' @param sort (character) The indexed field to sort by
#' @param order (character) Supports "asc" or "desc"
#' @param facets (list) Comma separated list of the fields to create facets
#' on e.g. facets=basis_of_record.
#' @param ... Further args passed on to \code{\link[httr]{GET}}.
#' @family nbn
#' @return a list with slots for metadata (`meta`) with list of response
#' attributes, and data (`data``) with a data.frame of results
#' @author Scott Chamberlain, \email{myrmecocystus@@gmail.com}
#' @references <https://api.nbnatlas.org/>
#'
#' @examples \dontrun{
#' x <- nbn_search(q = "Vulpes")
#' x$meta$totalRecords
#' x$meta$pageSize
#' x$meta$urlParameters
#' x$meta$queryTitle
#' head(x$data)
#'
#' nbn_search(q = "blackbird", start = 4)
#'
#' # debug curl stuff
#' library('httr')
#' nbn_search(q = "blackbird", config = verbose())
#' }
nbn_search <- function(q, order = NULL, sort = NULL, start = 0, rows = 25,
  facets = NULL, ...) {

  args <- tc(list(
    q = q, pageSize = rows, startIndex = start, sort = sort,
    dir = order, facets = facets
  ))
  nbn_GET(file.path(nbn_base(), "search"), args, ...)
}

nbn_GET <- function(url, args, ...){
  res <- GET(url, query = argsnull(args), ...)
  stop_for_status(res)
  tt <- con_utf8(res)
  json <- jsonlite::fromJSON(tt)$searchResults
  list(meta = pop(json, "results"), data = json$results)
}

nbn_base <- function() "https://species-ws.nbnatlas.org"
