# printhier.R

printhier <- function(hierout = NA) {
# Function to print taxnomic hierarchy
# Args:
#   hierout: output from a hierarchy search using get_itis_xml function (list)
# Output: 
# Examples:
#   out <- get_itis_xml("36616", "tsnfullhir", "tsn", FALSE)  
#   printhier(out)  
  page <- xmlTreeParse(hierout)
  templist <- ldply(xmlToList(page), function(x) data.frame(c(x[3], x[4])))[,-3]
  na.omit(templist)
}