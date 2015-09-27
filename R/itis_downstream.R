#' Retrieve all taxa names or TSNs downstream in hierarchy from given TSN.
#'
#' @export
#' @param tsns A taxonomic serial number.
#' @param downto The taxonomic level you want to go down to. See examples below.
#' 		The taxonomic level IS case sensitive, and you do have to spell it
#' 		correctly. See \code{data(rank_ref)} for spelling.
#' @param intermediate (logical) If TRUE, return a list of length two with target
#'    taxon rank names, with additional list of data.frame's of intermediate
#'    taxonomic groups. Default: FALSE
#' @param ... Further args passed on to \code{\link{gettaxonomicranknamefromtsn}} and
#'    \code{\link{gethierarchydownfromtsn}}
#' @return Data.frame of taxonomic information downstream to family from e.g.,
#' 		Order, Class, etc., or if \code{intermediated=TRUE}, list of length two,
#'   	with target taxon rank names, and intermediate names.
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @examples \dontrun{
#' ## the plant class Bangiophyceae, tsn 846509
#' itis_downstream(tsns = 846509, downto="Genus")
#' itis_downstream(tsns = 846509, downto="Genus", intermediate=TRUE)
#'
#' # get families downstream from Acridoidea
#' itis_downstream(tsns = 650497, "Family")
#' ## here, intermediate leads to the same result as the target
#' itis_downstream(tsns = 650497, "Family", intermediate=TRUE)
#'
#' # get species downstream from Ursus
#' itis_downstream(tsns = 180541, "Species")
#'
#' # get orders down from the Division Rhodophyta (red algae)
#' itis_downstream(tsns = 660046, "Order")
#' itis_downstream(tsns = 660046, "Order", intermediate=TRUE)
#'
#' # get tribes down from the family Apidae
#' itis_downstream(tsns = 154394, downto="Tribe")
#' itis_downstream(tsns = 154394, downto="Tribe", intermediate=TRUE)
#' }

itis_downstream <- function(tsns, downto, intermediate = FALSE, ...) {

	downto2 <- rank_ref[grep(downto, rank_ref$ranks),"rankId"]
	torank_ids <- rank_ref[grep(downto, rank_ref$ranks):nrow(rank_ref),"rankId"]

	stop_ <- "not"
	notout <- data.frame(rankName = "")
	out <- list()
  if (intermediate) intermed <- list()
	iter <- 0
	while (stop_ == "not") {
		iter <- iter + 1
		if (!nchar(as.character(notout$rankName[[1]])) > 0) {
			temp <- ldply(as.character(tsns), gettaxonomicranknamefromtsn)
		} else {
		  temp <- notout
		}
		tt <- ldply(as.character(temp$tsn), gethierarchydownfromtsn)
		## FIXME - do we need this since rank is given above in `tt`
		names_ <- ldply(split(tt, row.names(tt)), function(x) {
			gettaxonomicranknamefromtsn(as.character(x$tsn))[,c("rankId","rankName","tsn")]
		})
		##
		if (nrow(names_) == 0) {
		  out[[iter]] <- data.frame(tsn = "No data", parentname = "No data",
		                            parenttsn = "No data", taxonname = "No data",
		                            rankid = "No data", rankname = "No data")
		  stop_ <- "nodata"
		} else {
		  tt <- merge(tt[,-3], names_[,-1], by = "tsn")
		  if (intermediate) intermed[[iter]] <- tt
		  if (nrow(tt[tt$rankId == downto2, ]) > 0) out[[iter]] <- tt[tt$rankId == downto2, ]
		  if (nrow(tt[!tt$rankId == downto2, ]) > 0) {
		    notout <- tt[!tt$rankId %in% torank_ids, ]
		  } else {
		    notout <- data.frame(rankName = downto)
		  }

		  if (all(notout$rankName == downto)) {
		    stop_ <- "fam"
		  } else {
		    tsns <- notout$tsn
		    stop_ <- "not"
		  }
		}
		if (intermediate) intermed[[iter]] <- setNames(intermed[[iter]], tolower(names(intermed[[iter]])))
	}
  tmp <- ldply(out)
	if (intermediate) {
	  list(target = setNames(tmp, tolower(names(tmp))), intermediate = intermed)
  } else {
    setNames(tmp, tolower(names(tmp)))
  }
}
