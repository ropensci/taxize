# Function to get family names to make Phylomatic input object
# input: x = quoted tsn number (taxonomic serial number)
# output: family name as character
get_familyname <- function (x) {
  temp <- get_itis_xml(searchterm = x, searchtype = "tsnfullhir", by_ = "tsn")
  templist <- ldply(xmlToList(temp), function(x) data.frame(c(x[3], x[4])))[,-3]
  hier <- na.omit(templist)
  famname <- as.character(hier[hier$rankName == 'Family',2])
  return(famname)
}