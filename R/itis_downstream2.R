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
#' itis_downstream2(tsns = 650497) # getting families downstream from Acridoidea
#' itis_downstream2(tsns = 114989) # getting families downstream from Strepsiptera
#' itis_downstream2(tsns = 99208) # getting families downstream from Class Insecta
#' }
#' @export
itis_downstream2 <- function(tsns) {
	lets <- c(LETTERS, paste(LETTERS, LETTERS, sep=""), 
						paste(LETTERS, LETTERS, LETTERS, sep=""), 
						paste(LETTERS, LETTERS, LETTERS, LETTERS, sep=""), 
						paste(LETTERS, LETTERS, LETTERS, LETTERS, LETTERS, sep=""),
						paste(LETTERS, LETTERS, LETTERS, LETTERS, LETTERS, LETTERS, sep=""),
						paste(LETTERS, LETTERS, LETTERS, LETTERS, LETTERS, LETTERS, LETTERS, sep=""))
	fam <- "not" 
	notout <- data.frame(rankName = "")
	out <- list()
	while(fam == "not"){
		if(!nchar(as.character(notout$rankName[[1]])) > 0){
			temp <- ldply(tsns, gettaxonomicranknamefromtsn)
		} else
			{ temp <- notout }
		tt <- ldply(temp$tsn, gethierarchydownfromtsn)
		names_ <- ldply(split(tt, row.names(tt)), function(x) 
			gettaxonomicranknamefromtsn(x$tsn)[,c("rankName","tsn")]) 
		tt <- merge(tt[,-3], names_[,-1], by="tsn")
		if(nrow(tt[tt$rankName == "Family", ]) > 0) out[[sample(lets, 1)]] <- tt[tt$rankName == "Family", ]
		if(nrow(tt[!tt$rankName == "Family", ]) > 0) {
			notout <- tt[!tt$rankName %in% c('Family','Subfamily','Tribe','Subtribe','Genus',
				'Subgenus','Species','Subspecies','Variety','Infrakingdom','Division',
				'Subdivision','Infradivision','Section','Subsection','Subvariety','Form',
				'Subform','Race','Stirp','Morph','Aberration','Unspecified'
			), ]
		} else
			{ notout <- data.frame(rankName = "Family") }
		
		if(all(notout$rankName == "Family")) { 
			fam <- "fam"
		} else
			{ 
				tsns <- notout$tsn
				fam <- "not" 
			}
	}
	ldply(out)[,-1]
}