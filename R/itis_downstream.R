#' Retrieve all taxa names or TSNs downstream in hierarchy from given TSN.
#' 
#' @import XML RCurl plyr
#' @param tsns A taxonomic serial number. 
#' @param downto The taxonomic level you want to go down to. See examples below.
#' 		The taxonomic level IS case sensitive, and you do have to spell it 
#' 		correctly. See \code{data(rank_ref)} for spelling.
#' @param locally If TRUE, queries are run locally in sqlite3; if FALSE (the default), 
#' 		queries are run against the ITIS web API. locally=TRUE should be faster in almost all cases.
#' @return Data.frame of taxonomic information downstream to family from e.g., 
#' 		Order, Class, etc. 
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @examples \dontrun{
#' itis_downstream(tsns=846509, downto="Genus")
#' itis_downstream(tsns = 650497, "Family") # getting families downstream from Acridoidea
#' itis_downstream(tsns = 180541, "Species") # getting species downstream from Ursus
#' }
#' @export
itis_downstream <- function(tsns, downto, locally = FALSE) 
{
	downto2 <- rank_ref[grep(downto, rank_ref$ranks),"rankId"]
	torank_ids <- rank_ref[grep(downto, rank_ref$ranks):nrow(rank_ref),"rankId"]
	
	stop_ <- "not" 
	notout <- data.frame(rankName = "")
	out <- list()
	iter <- 0
	while(stop_ == "not"){
		iter <- iter + 1
		if(!nchar(as.character(notout$rankName[[1]])) > 0){
			temp <- ldply(tsns, gettaxonomicranknamefromtsn, locally = locally)
		} else
			{ temp <- notout }
		tt <- ldply(temp$tsn, gethierarchydownfromtsn, locally = locally)
		names_ <- ldply(split(tt, row.names(tt)), function(x) 
			gettaxonomicranknamefromtsn(x$tsn, locally = locally)[,c("rankId","rankName","tsn")]) 
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