#' Retrieve all taxa names or TSNs downstream in hierarchy from given TSN.
#' 
#' @import XML RCurl ritis plyr
#' @param tsns A taxonomic serial number. 
#' @param downto The taxonomic level you want to go down to. See examples below.
#' 		The taxonomic level is not case sensitive but you do have to spell it 
#' 		correctly. See \code{data(rank_ref)} for spelling.
#' @return Data.frame of taxonomic information downstream to family from e.g., 
#' 		Order, Class, etc. 
#' @author Scott Chamberlain {myrmecocystus@@gmail.com}
#' @examples \dontrun{
#' downstream2(846509, "Genus")
#' downstream2(tsns = 650497, "Family") # getting families downstream from Acridoidea
#' downstream2(tsns = 180541, "Species") # getting species downstream from Ursus
#' }
#' @export
downstream2 <- function(tsns, downto) 
{
	# load rank reference data.frame
	if(exists(as.character(substitute(rank_ref)))==TRUE){ NULL } else
		{ data(rank_ref); message("loaded rank_ref") }
	downto2 <- rank_ref[grep(downto, rank_ref$ranks, ignore.case=T),"rankId"]
	torank_ids <- rank_ref[grep(downto, rank_ref$ranks, ignore.case=T):nrow(rank_ref),"rankId"]
	
	stop_ <- "not" 
	notout <- data.frame(rankName = "")
	out <- list()
	iter <- 0
	while(stop_ == "not"){
		iter <- iter + 1
		if(!nchar(as.character(notout$rankName[[1]])) > 0){
			temp <- ldply(tsns, gettaxonomicranknamefromtsn)
		} else
			{ temp <- notout }
		tt <- ldply(temp$tsn, gethierarchydownfromtsn)
		names_ <- ldply(split(tt, row.names(tt)), function(x) 
			gettaxonomicranknamefromtsn(x$tsn)[,c("rankId","rankName","tsn")]) 
		tt <- merge(tt[,-3], names_[,-1], by="tsn")
		if(nrow(tt[tt$rankId == downto2, ]) > 0) out[[iter]] <- tt[tt$rankId == downto2, ]
		if(nrow(tt[!tt$rankId == downto2, ]) > 0) {
			notout <- tt[!tt$rankId %in% torank_ids, ]
		} else
			{ notout <- data.frame(rankName = downto) }
		
		if(all(notout$rankName == downto)) { 
			stop_ <- "fam"
		} else
		{ 
			tsns <- notout$tsn
			stop_ <- "not" 
		}
	}
	ldply(out)
}