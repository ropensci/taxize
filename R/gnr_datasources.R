#' Global Names Resolver Data Sources
#'
#' Retrieve data sources used in the Global Names Resolver
#'
#' @export
#' @param ... Curl options passed on to \code{\link[crul]{HttpClient}}
#' @param todf defunct, always get a data.frame back now
#' @return data.frame/tibble
#' @seealso \code{\link[taxize]{gnr_resolve}}, \code{\link[taxize]{gni_search}}
#' @keywords resolve names taxonomy
#' @references https://resolver.globalnames.org/data_sources
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
  res <- tax_GET(url = "https://resolver.globalnames.org",
    path = "data_sources.json", ...)
  tibble::as_tibble(jsonlite::fromJSON(res$parse("UTF-8")))
}
