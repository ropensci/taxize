#' Retrieve all taxa names or TSNs downstream in hierarchy from given TSN.
#' 
#' THE FUNCTION NEEDS TO BE GENERALIZED TO ANY RANK DESIRED IN THE OUTPUT
#' 		JUST WORKS FOR GOING DOWN TO FAMILY RIGHT NOW.
#' 
#' @import XML RCurl ritis plyr
#' @param tsns A taxonomic serial number. 
#' @return Data.frame of taxonomic information downstream to family from e.g., 
#' 		Order, Class, etc. 
#' @author Scott Chamberlain {myrmecocystus@@gmail.com}
#' @examples \dontrun{
#' downstream2(114989) # getting families downstream from Strepsiptera
#' downstream2(650497) # getting families downstream from Acridoidea
#' }
#' @export
downstream2 <- function(tsns) {
	fam <- "not" 
	out <- list()
	while(fam == "not"){
		temp <- ldply(tsns, gettaxonomicranknamefromtsn)
		tt <- ldply(temp$tsn, gethierarchydownfromtsn)
		names_ <- laply(split(tt, row.names(tt)), function(x) 
			as.character(gettaxonomicranknamefromtsn(x$tsn)$rankName) 
		)
		tt$rankName <- names_
		if(nrow(tt[tt$rankName == "Family", ]) > 0) out[[fam]] <- tt[tt$rankName == "Family", ]
		if(nrow(tt[!tt$rankName == "Family", ]) > 0) {
			notout <- tt[!tt$rankName == "Family", ]
			notout <- notout[!notout %in% c("Subfamily","Tribe","Genus","Species","Subspecies"), ]
		} else
		{ notout <- data.frame(rankName = "Family") }
		
		if(all(notout$rankName == "Family")) { 
			fam <- "fam"
		} else
		{ 
			tsns <- tt$tsn
			fam <- "not" 
		}
	}
	out
}