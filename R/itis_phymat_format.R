#' Get family names to make Phylomatic input object, and output input string 
#'    to Phylomatic for use in the function phylomatic_tree.
#'    
#' @import XML RCurl stringr plyr
#' @param taxa quoted tsn number (taxonomic serial number)
#' @param format output format, isubmit (you can paste in to the Phylomatic 
#'     website), or 'rsubmit' to use in fxn phylomatic_tree
#' @param db One of ncbi or itis
#' @return e.g., "pinaceae/pinus/pinus_contorta", in Phylomatic submission format.
#' @examples \dontrun{
#' laply(c("Poa annua", "Abies procera", "Helianthus annuus"), 
#'    itis_phymat_format, format='rsubmit')
#' }
#' @export
itis_phymat_format <- function(taxa = NA, format='isubmit', db="ncbi") 
{
	family <- tax_name(query=taxa, get="family", db=db)
	stringg <- c(family, strsplit(taxa, " ")[[1]])
	stringg <- tolower(as.character(stringg))
	if (format == 'isubmit') {
		paste(stringg[[1]], "/", stringg[2], "/", tolower(str_replace(taxa, " ", "_")), sep='')
	} else
		if (format == 'rsubmit') {
			paste(stringg[[1]], "%2F", stringg[2], "%2F", tolower(str_replace(taxa, " ", "_")), sep='')
		} 
# 	tt <- getfullhierarchyfromtsn(tsn)
# 	tt_ <- tt[tt$rankName %in% c("Family","Genus","Species"), "taxonName"]
# 	tt__ <- tolower(as.character(tt_))
#   if (format == 'isubmit') {
#     paste(tt__[1], "/", tt__[2], "/", str_replace(tt__[3], " ", "_"), sep='')
#   } else
#   if (format == 'rsubmit') {
#     paste(tt__[1], "%2F", tt__[2], "%2F", str_replace(tt__[3], " ", "_"), sep='')
#   } 
}