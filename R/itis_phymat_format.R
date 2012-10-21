#' Get family names to make Phylomatic input object, and output input string 
#'    to Phylomatic for use in the function phylomatic_tree.
#'    
#' @import XML RCurl stringr plyr
#' @param tsn quoted tsn number (taxonomic serial number)
#' @param format output format, isubmit (you can paste in to the Phylomatic 
#     website), or 'rsubmit' to use in fxn phylomatic_tree
#' @return e.g., "pinaceae/pinus/pinus_contorta", in Phylomatic submission format.
#' @examples \dontrun{
#' dat_ <- laply(list("36616", "19322", "183327"), itis_phymat_format, format='rsubmit')
#' tree <- phylomatic_tree(dat_, 'GET', 'new', 'TRUE')
#' plot(tree)
#' }
#' @export
itis_phymat_format <- function(tsn = NA, format) 
{
	tt <- getfullhierarchyfromtsn(tsn)
	tt_ <- tt[tt$rankName %in% c("Family","Genus","Species"), "taxonName"]
	tt__ <- tolower(as.character(tt_))
  if (format == 'isubmit') {
    paste(tt__[1], "/", tt__[2], "/", str_replace(tt__[3], " ", "_"), sep='')
  } else
  if (format == 'rsubmit') {
    paste(tt__[1], "%2F", tt__[2], "%2F", str_replace(tt__[3], " ", "_"), sep='')
  } 
}