#' Ping EOL API to see if it's working.
#' @import XML RCurl
#' @return XML object message about API status.
#' @export
#' @examples \dontrun{
#' eolping()
#' }
eolping <- 

function(url = 'http://www.eol.org/api/ping')
{
  xmlToList(xmlTreeParse(getURL(url)))$message
}