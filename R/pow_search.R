#' Search Kew's Plants of the World
#'
#' @export
#' @param q (character) query terms
#' @param limit (integer) Number of records to return. default: 100
#' @param cursor (character) cursor string
#' @param sort (character) The field to sort by and sort order separted with 
#' underscore, e.g., `sort="name_desc"`
#' @param ... Further args passed on to [crul::HttpClient].
#' @return a list with slots for metadata (`meta`) with list of response
#' attributes, and data (`data`) with a data.frame of results
#' @author Scott Chamberlain, \email{myrmecocystus@@gmail.com}
#' @references <http://powo.science.kew.org/>
#' @family pow
#' @examples \dontrun{
#' x <- pow_search(q = "Quercus")
#' x$meta
#' x$meta$totalResults
#' x$meta$perPage
#' x$meta$totalPages
#' x$meta$page
#' x$meta$cursor
#' head(x$data)
#'
#' # pagination
#' pow_search(q = "sunflower", limit = 2)
#'
#' # debug curl stuff
#' invisible(pow_search(q = "Helianthus annuus", verbose = TRUE))
#' 
#' # sort
#' desc <- pow_search(q = "Helianthus", sort = "name_desc")
#' desc$data$name
#' asc <- pow_search(q = "Helianthus", sort = "name_asc")
#' asc$data$name
#' }
pow_search <- function(q, limit = 100, cursor = "*", sort = NULL, ...) {
  assert(q, "character")
  assert(limit, c("integer", "numeric"))
  assert(cursor, "character")
  assert(sort, "character")
  args <- tc(list(q = q, perPage = limit, cursor = cursor, sort = sort))
  pow_GET(file.path(pow_base(), "api/2", "search"), args, ...)
}

#' Lookup taxa in Kew's Plants of the World
#' 
#' @export
#' @param id (character) taxon id. required
#' @param include (character) vector of additional fields to include in 
#' results. options include 'distribution' and 'descriptions'. optional
#' @param ... Further args passed on to [crul::HttpClient].
#' @family pow
#' @examples \dontrun{
#' pow_lookup(id = 'urn:lsid:ipni.org:names:320035-2')
#' pow_lookup(id = 'urn:lsid:ipni.org:names:320035-2', 
#'   include = "distribution")
#' pow_lookup(id = 'urn:lsid:ipni.org:names:320035-2', 
#'   include = c("distribution", "descriptions"))
#' }
pow_lookup <- function(id, include = NULL, ...) {
  assert(include, "character")
  if (!is.null(include)) {
    if (!include %in% pow_include_fields) {
      stop("'include' must be one of ", paste0(pow_include_fields, collapse=","))
    }
  }
  if (!is.null(include)) include <- paste0(include, collapse = ",")
  args <- tc(list(fields = include))
  pow_GET(file.path(pow_base(), "api/2/taxon", id), args, ...)
}

pow_include_fields <- c("distribution", "descriptions")

pow_GET <- function(url, args, ...){
  cli <- crul::HttpClient$new(url = url, 
                              headers = tx_ual, opts = list(...))
  tt <- cli$get(query = argsnull(args))
  tt$raise_for_status()
  json <- jsonlite::fromJSON(tt$parse("UTF-8"))
  list(meta = pop(json, "results"), data = json$results)
}

pow_base <- function() "http://www.plantsoftheworldonline.org"
