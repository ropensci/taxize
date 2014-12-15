#' Ping EOL API to see if it's working.
#'
#' @import XML RCurl
#' @export
#' @return XML object message about API status.
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @seealso \code{\link{itis_ping}}
#' @examples \dontrun{
#' eol_ping()
#' }
eol_ping <- function(...) {
  res <- GET('http://eol.org/api/ping', ...)
  grepl("success", xmlToList(content(res))$message, ignore.case = TRUE)
}
