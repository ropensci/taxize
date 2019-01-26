#' EUBON capabilities
#'
#' @export
#' @param ... Curl options passed on to \code{\link[crul]{verb-GET}}
#' @references \url{http://cybertaxonomy.eu/eu-bon/utis/1.2/doc.html}
#' @family eubon-methods
#' @examples \dontrun{
#' eubon_capabilities()
#' }
eubon_capabilities <- function(...) {
  res <- crul::HttpClient$new(file.path(eubon_base(), "capabilities"),
    opts = list(...))$get()
  res$raise_for_status()
  tibble::as_tibble(
    jsonlite::fromJSON(res$parse("UTF-8"), TRUE, flatten = TRUE)
  )
}
