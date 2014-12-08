#' Ping the ITIS API to see if it's working.
#'
#' And provides number of scientific and common names in a string.
#'
#' @examples \dontrun{
#' itis_ping()
#' }
#' @seealso \code{\link{eol_ping}}
#' @export
itis_ping <- function() getdescription()
