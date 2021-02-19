#' Search Kew's Plants of the World
#'
#' @export
#' @param sci_com (character) query terms, scientific or common name
#' @param limit (integer) Number of records to return. default: 100
#' @param cursor (character) cursor string
#' @param sort (character) The field to sort by and sort order separted with
#' underscore, e.g., `sort="name_desc"`
#' @param q Deprecated, see `sci_com`
#' @param ... Further args passed on to [crul::HttpClient].
#' @return a list with slots for metadata (`meta`) with list of response
#' attributes, and data (`data`) with a data.frame of results
#' @author Scott Chamberlain, 
#' @references http://powo.science.kew.org/
#' @family pow
#' @examples \dontrun{
#' x <- pow_search(sci_com = "Quercus")
#' x$meta
#' x$meta$totalResults
#' x$meta$perPage
#' x$meta$totalPages
#' x$meta$page
#' x$meta$cursor
#' head(x$data)
#'
#' # pagination
#' pow_search(sci_com = "sunflower", limit = 2)
#'
#' # debug curl stuff
#' invisible(pow_search(sci_com = "Helianthus annuus", verbose = TRUE))
#'
#' # sort
#' desc <- pow_search(sci_com = "Helianthus", sort = "name_desc")
#' desc$data$name
#' asc <- pow_search(sci_com = "Helianthus", sort = "name_asc")
#' asc$data$name
#' }
pow_search <- function(sci_com, limit = 100, cursor = "*", sort = NULL,
  q = NULL, ...) {

  assert(sci_com, "character")
  assert(limit, c("integer", "numeric"))
  assert(cursor, "character")
  assert(sort, "character")
  pchk(q, "sci_com")
  args <- tc(list(q = sci_com, perPage = limit, cursor = cursor, sort = sort))
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
    if (!all(include %in% pow_include_fields)) {
      stop("'include' must be one of ", paste0(pow_include_fields, collapse=","))
    }
  }
  if (!is.null(include)) include <- paste0(include, collapse = ",")
  args <- tc(list(fields = include))
  pow_GET(file.path(pow_base(), "api/2/taxon", id), args, ...)
}

#' Lookup synonyms in Kew's Plants of the World
#'
#' @export
#' @param id (character) taxon id. required
#' @param ... Further args passed on to [pow_lookup()]
#' @family pow
#' @examples \dontrun{
#' pow_synonyms(id = 'urn:lsid:ipni.org:names:320035-2')
#' pow_synonyms(id = 'urn:lsid:ipni.org:names:358881-1')
#' pow_synonyms(id = 'urn:lsid:ipni.org:names:359855-1')
#' }
pow_synonyms <- function(id, ...) {
  res <- pow_lookup(id, ...)
  if ("synonyms" %in% names(res$meta)) {
    tibble::as_tibble(res$meta$synonyms)
  } else {
    tibble::tibble()
  }
}

pow_include_fields <- c("distribution", "descriptions")

pow_GET <- function(url, args, ...){
  cli <- crul::HttpClient$new(url = url,
                              headers = tx_ual, opts = list(...))
  tt <- cli$get(query = argsnull(args))
  tt$raise_for_status()
  json <- jsonlite::fromJSON(tt$parse("UTF-8"))
  meta <- pop(json, "results")
  meta$message <- NULL
  list(meta = meta, data = json$results)
}

pow_base <- function() "http://www.plantsoftheworldonline.org"
