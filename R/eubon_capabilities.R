#' EUBON capabilites
#'
#' @export
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @references \url{http://cybertaxonomy.eu/eu-bon/utis/1.2/doc.html}
#' @family eubon-methods
#' @examples \dontrun{
#' eubon_capabilities()
#' }
eubon_capabilities <- function(...) {
  res <- httr::GET(file.path(eubon_base(), "capabilities"), ...)
  httr::stop_for_status(res)
  tibble::as_data_frame(
    jsonlite::fromJSON(con_utf8(res), TRUE, flatten = TRUE)
  )
}
