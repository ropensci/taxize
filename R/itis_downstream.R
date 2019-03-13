#' Retrieve all taxa names or TSNs downstream in hierarchy from given TSN.
#'
#' @export
#' @param tsns A taxonomic serial number.
#' @param downto The taxonomic level you want to go down to. See examples below.
#' 		The taxonomic level IS case sensitive, and you do have to spell it
#' 		correctly. See `data(rank_ref)` for spelling.
#' @param intermediate (logical) If TRUE, return a list of length two with target
#'    taxon rank names, with additional list of data.frame's of intermediate
#'    taxonomic groups. Default: FALSE
#' @param ... Further args passed on to [`ritis::rank_name()`] and
#' [`ritis::hierarchy_down()`]
#' @return Data.frame of taxonomic information downstream to family from e.g.,
#' 		Order, Class, etc., or if `intermediated=TRUE`, list of length two,
#'   	with target taxon rank names, and intermediate names.
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @examples \dontrun{
#' ## the plant class Bangiophyceae, tsn 846509
#' itis_downstream(tsns = 846509, downto="genus")
#' itis_downstream(tsns = 846509, downto="genus", intermediate=TRUE)
#'
#' # get families downstream from Acridoidea
#' itis_downstream(tsns = 650497, "family")
#' ## here, intermediate leads to the same result as the target
#' itis_downstream(tsns = 650497, "family", intermediate=TRUE)
#'
#' # get species downstream from Ursus
#' itis_downstream(tsns = 180541, "species")
#'
#' # get orders down from the Division Rhodophyta (red algae)
#' itis_downstream(tsns = 660046, "order")
#' itis_downstream(tsns = 660046, "order", intermediate=TRUE)
#'
#' # get tribes down from the family Apidae
#' itis_downstream(tsns = 154394, downto="tribe")
#' itis_downstream(tsns = 154394, downto="tribe", intermediate=TRUE)
#' }

itis_downstream <- function(tsns, downto, intermediate = FALSE, ...) {
  downto <- tolower(downto)
  downto2 <- rank_ref[which_rank(downto), "rankid"]
  torank_ids <- rank_ref[which_rank(downto):NROW(rank_ref), "rankid"]

	stop_ <- "not"
	notout <- data.frame(rankname = "")
	out <- list()
	if (intermediate) intermed <- list()
	iter <- 0
	while (stop_ == "not") {
		iter <- iter + 1
		if (!nchar(as.character(notout$rankname[[1]])) > 0) {
			temp <- ldply(as.character(tsns), ritis::rank_name)
		} else {
		  temp <- notout
		}
		tt <- ldply(as.character(temp$tsn), ritis::hierarchy_down)
		## FIXME - do we need this since rank is given above in `tt`
		names_ <- ldply(split(tt, row.names(tt)), function(x) {
		  ritis::rank_name(as.character(x$tsn))[,c("rankid","rankname","tsn")]
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
		  if (nrow(tt[tt$rankid == downto2, ]) > 0) out[[iter]] <- tt[tt$rankid == downto2, ]
		  if (nrow(tt[!tt$rankid == downto2, ]) > 0) {
		    notout <- tt[!tt$rankid %in% torank_ids, ]
		  } else {
		    notout <- data.frame(rankname = downto)
		  }

		  if (all(notout$rankname == downto)) {
		    stop_ <- "fam"
		  } else {
		    tsns <- notout$tsn
		    stop_ <- "not"
		  }
		}
		if (intermediate) {
		  intermed[[iter]] <- stats::setNames(intermed[[iter]], tolower(names(intermed[[iter]])))
		  intermed[[iter]]$rankname <- tolower(intermed[[iter]]$rankname)
		}
	}
  tmp <- ldply(out)
  tmp$rankname <- tolower(tmp$rankname)
	if (intermediate) {
	  list(target = stats::setNames(tmp, tolower(names(tmp))), intermediate = intermed)
  } else {
    stats::setNames(tmp, tolower(names(tmp)))
  }
}
