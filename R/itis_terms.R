#' Get ITIS terms, i.e., tsn's, authors, common names, and scientific names.
#'
#' @export
#' @param query One or more common or scientific names, or partial names
#' @param what One of both (search common and scientific names), common
#' (search just common names), or scientific (search just scientific names)
#' @param ... Further arguments passed on to [ritis::terms()]
#' @examples \dontrun{
#' # Get terms searching both common and scientific names
#' itis_terms(query='bear')
#'
#' # Get terms searching just common names
#' itis_terms(query='tarweed', "common")
#'
#' # Get terms searching just scientific names
#' itis_terms(query='Poa annua', "scientific")
#' }
itis_terms <- function(query, what = "both", ...) {
  if (!what %in% c('both', 'scientific', 'common')) {
    stop("what must be one of 'both', 'scientific', 'common'", call. = FALSE)
  }
  temp <- switch(
    what,
    both = lapply(query, ritis::terms, ...),
    common = lapply(query, ritis::terms, what = "common", ...),
    scientific = lapply(query, ritis::terms, what = "scientific", ...)
  )
  if (length(query) == 1) {
    temp[[1]]
  } else {
    stats::setNames(temp, query)
  }
}
