#' Global Names Resolver Data Sources
#'
#' Retrieve data sources used in the Global Names Resolver
#'
#' @export
#' @param ... Curl options passed on to [crul::HttpClient]
#' @param todf defunct, always get a data.frame back now
#' @return data.frame/tibble
#' @seealso [gnr_resolve()], [gni_search()]
#' @keywords resolve names taxonomy
#' @references <https://resolver.globalnames.org/data_sources>
#' @examples \dontrun{
#' # all data sources
#' gnr_datasources()
#'
#' # give me the id for EOL
#' out <- gnr_datasources()
#' out[out$title == "EOL", "id"]
#'
#' # Fuzzy search for sources with the word zoo
#' out <- gnr_datasources()
#' out[agrep("zoo", out$title, ignore.case = TRUE), ]
#' }
gnr_datasources <- function(..., todf) {
  if (!missing(todf)) stop("todf is defunct", call. = FALSE)
  cli <- crul::HttpClient$new(
    url = "https://resolver.globalnames.org/data_sources.json",
    headers = tx_ual, opts = list(...))
  res <- cli$get()
  res$raise_for_status()
  tibble::as_tibble(jsonlite::fromJSON(res$parse("UTF-8")))
}
