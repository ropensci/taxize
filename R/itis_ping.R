#' Ping the ITIS API to see if it's working.
#'
#' And provides number of scientific and common names in a string.
#' @export
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @seealso \code{\link{eol_ping}}
#' @examples \dontrun{
#' itis_ping()
#' }
itis_ping <- function(...) getdescription(...)
