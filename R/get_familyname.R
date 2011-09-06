# Function to get family names to make Phylomatic input object
# input: x = quoted tsn number (taxonomic serial number)
# output: family name as character
get_familyname <- function (x) {
  temp <- get_itis_xml(searchterm = x, searchtype = "tsnfullhir", by_ = "tsn")
  tempdf <- ldply(xmlToList(temp)$return, function(x) data.frame(c(x[3], x[4])))
  famname <- as.character(tempdf[tempdf$rankName == 'Family', 3])[1]
  return(famname)
}