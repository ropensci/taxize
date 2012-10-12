#' Ping EOL API to see if it's working.
#' 
#' @import XML RCurl
#' @return XML object message about API status.
#' @export
#' @examples \dontrun{
#' eol_ping()
#' }
eol_ping <- function()
{
  xmlToList(xmlTreeParse(getURL('http://eol.org/api/ping')))$message
}