# eolping.R

eolping <- 
# Args:
#   NONE
# Examples:
#   eolping()

function(url = 'http://www.eol.org/api/ping')
{
  message <- xmlToList(xmlTreeParse(getURL(url)))$message
  return(message)
}