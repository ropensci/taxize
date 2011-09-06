# Function to get family names to make Phylomatic input object
# AND output input string to Phylomatic for use in the function get_phylomatic_tree
# input: x = quoted tsn number (taxonomic serial number), format = one of 'isubmit' 
# or 'rsubmit'
# output: family name as character
# x <- "183327"
get_phymat_format <- function (x, format) {
  temp <- get_itis_xml(searchterm = x, searchtype = "tsnfullhir", by_ = "tsn")
  tempdf <- ldply(xmlToList(temp)$return, function(x) data.frame(c(x[3], x[4])))[,-4]
  hier <- na.omit(tempdf)
#   names <- tolower(as.character(hier[hier$rankName == c('Family', 'Genus', 'Species'), 2]))
  names <- tolower(c(
    as.character(hier[hier$rankName == 'Family', 3]),
    as.character(hier[hier$rankName == 'Genus', 3]),
    as.character(hier[hier$rankName == 'Species', 3])))
  if (format == 'isubmit') {
    dat <- paste(names[1], "/", names[2], "/", str_replace(names[3], " ", "_"), sep='')
  } else
  if (format == 'rsubmit') {
    dat <- paste(names[1], "%2F", names[2], "%2F", str_replace(names[3], " ", "_"), sep='')
  } 
  end
return(dat)
}
