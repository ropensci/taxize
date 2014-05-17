#' Retrieve all taxa names or TSNs downstream in hierarchy from given TSN.
#' 
#' @import XML RCurl plyr
#' @param tsns A taxonomic serial number. 
#' @param downto The taxonomic level you want to go down to. See examples below.
#' 		The taxonomic level IS case sensitive, and you do have to spell it 
#' 		correctly. See \code{data(rank_ref)} for spelling.
#' @param verbose logical; If TRUE (default), informative messages printed.
#' @return Data.frame of taxonomic information downstream to family from e.g., 
#' 		Order, Class, etc. 
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @examples \dontrun{
#' itis_downstream(tsns = 846509, downto="Genus")
#' 
#' # getting families downstream from Acridoidea
#' itis_downstream(tsns = 650497, "Family")
#' 
#' # getting species downstream from Ursus
#' itis_downstream(tsns = 180541, "Species")
#' }
#' @export
itis_downstream <- function(tsns, downto, verbose=TRUE) 
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
			temp <- ldply(tsns, function(z) gettaxonomicranknamefromtsn(z, verbose=verbose))
		} else
			{ temp <- notout }
		tt <- ldply(temp$tsn, function(z) gethierarchydownfromtsn(z, verbose=verbose))
		names_ <- ldply(split(tt, row.names(tt)), function(x) 
			gettaxonomicranknamefromtsn(x$tsn, verbose=verbose)[,c("rankId","rankName","tsn")])
		if(nrow(names_) == 0){
		  out[[iter]] <- data.frame(tsn="No data",parentname="No data",parenttsn="No data",taxonname="No data",rankid="No data",rankname="No data")
		  stop_ <- "nodata"
		} else {
		  tt <- merge(tt[,-3], names_[,-1], by="tsn")
		  if(nrow(tt[tt$rankId == downto2, ]) > 0) out[[iter]] <- tt[tt$rankId == downto2, ]
		  if(nrow(tt[!tt$rankId == downto2, ]) > 0) {
		    notout <- tt[!tt$rankId %in% torank_ids, ]
		  } else { 
		    notout <- data.frame(rankName = downto) 
		  }
		  
		  if(all(notout$rankName == downto)) { 
		    stop_ <- "fam"
		  } else
		  { 
		    tsns <- notout$tsn
		    stop_ <- "not" 
		  }
		}
	}
	tmp <- ldply(out)
  names(tmp) <- tolower(names(tmp))
  tmp
}