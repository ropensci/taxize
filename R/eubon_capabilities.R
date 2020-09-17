#' EUBON capabilities
#'
#' @export
#' @param ... Curl options passed on to [crul::verb-GET]
#' @references https://cybertaxonomy.eu/eu-bon/utis/1.3/doc.html
#' @family eubon-methods
#' @examples \dontrun{
#' eubon_capabilities()
#' }
eubon_capabilities <- function(...) {
  res <- crul::HttpClient$new(file.path(eubon_base(), "capabilities"),
    headers = tx_ual, opts = list(...))$get()
  res$raise_for_status()
  tibble::as_tibble(
    jsonlite::fromJSON(res$parse("UTF-8"), TRUE, flatten = TRUE)
  )
}
