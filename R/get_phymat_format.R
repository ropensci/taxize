#' Get family names to make Phylomatic input object, and output input string 
#'    to Phylomatic for use in the function get_phylomatic_tree.
#'    
#' @import XML RCurl stringr plyr
#' @param tsn quoted tsn number (taxonomic serial number)
#' @param format output format, isubmit (you can paste in to the Phylomatic 
#     website), or 'rsubmit' to use in fxn get_phylomatic_tree
#' @return e.g., "pinaceae/pinus/pinus_contorta", in Phylomatic submission format.
#' @examples \dontrun{
#' dat_ <- laply(list("36616", "19322", "183327"), get_phymat_format, 
#'       format='rsubmit', .progress="text")
#' dat_mine <- paste(dat_, collapse="%0D%0A") # collapse and replace \n's
#' tree <- get_phylomatic_tree(dat_mine, 'FALSE', 'GET', 'new', 'TRUE')
#' plot(tree)
#' }
#' @export
get_phymat_format <- function(tsn = NA, format) 
{
  temp <- get_itis_xml(searchterm = tsn, searchtype = "tsnfullhir", by_ = "tsn")
  xmllist <- xmlToList(temp)$return[-1]
  tempdf <- ldply(xmllist, function(x) data.frame(c(x[4], x[5])))[,-4]
  hier <- na.omit(tempdf)
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
dat
}