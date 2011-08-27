# uBio searching
require(base64)

url <- 'http://www.ubio.org/webservices/service.php?function='
url <- 'http://www.ubio.org/webservices/service.php?function=namebank_search&searchName=elephant&sci=1&vern=1&keyCode=b052625da5f330e334471f8efe725c07bf4630a6'
temp <- getURL(url)
xmltemp <- xmlParse(temp)
xmlToList(xmltemp)

tempout <- 
decode(input=temp)
str(temp)

base64(temp)
class(temp)

txt = "Some simple text for base 64 to handle"
x = base64(txt)
llply(xmlToList(xmltemp),  base64Decode )

class("U21pdGgncyBSb2NrIEVsZXBoYW50IFNocmV3")