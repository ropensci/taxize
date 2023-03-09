#' Retrieve all taxa names or TSNs downstream in hierarchy from given TSN.
#'
#' @export
#' @param id A taxonomic serial number.
#' @param downto The taxonomic level you want to go down to. See examples below.
#' The taxonomic level IS case sensitive, and you do have to spell it
#' correctly. See `data(rank_ref)` for spelling.
#' @param intermediate (logical) If TRUE, return a list of length two with target
#' taxon rank names, with additional list of data.frame's of intermediate
#' taxonomic groups. Default: FALSE
#' @param tsns Deprecated, see `id`
#' @param ... Further args passed on to [ritis::rank_name()] and
#' [ritis::hierarchy_down()]
#' @return Data.frame of taxonomic information downstream to family from e.g.,
#' Order, Class, etc., or if `intermediated=TRUE`, list of length two,
#' with target taxon rank names, and intermediate names.
#' @examples \dontrun{
#' ## the plant class Bangiophyceae, tsn 846509
#' itis_downstream(id = 846509, downto="genus")
#' itis_downstream(id = 846509, downto="genus", intermediate=TRUE)
#'
#' # get families downstream from Acridoidea
#' itis_downstream(id = 650497, "family")
#' ## here, intermediate leads to the same result as the target
#' itis_downstream(id = 650497, "family", intermediate=TRUE)
#'
#' # get species downstream from Ursus
#' itis_downstream(id = 180541, "species")
#'
#' # get orders down from the Division Rhodophyta (red algae)
#' itis_downstream(id = 660046, "order")
#' itis_downstream(id = 660046, "order", intermediate=TRUE)
#'
#' # get tribes down from the family Apidae
#' itis_downstream(id = 154394, downto="tribe")
#' itis_downstream(id = 154394, downto="tribe", intermediate=TRUE)
#' }

itis_downstream <- function(id, downto, intermediate = FALSE, tsns = NULL,
	...) {

  if (!is.null(tsns)) {
    lifecycle::deprecate_warn(when = "v0.9.97", what = "itis_downstream(tsns)", with = "itis_downstream(id)")
    id <- tsns
  }
  
  if (!is.null(tsns)) id <- tsns
  downto <- tolower(downto)
  downto2 <- taxize_ds$rank_ref[which_rank(downto), "rankid"]
  torank_ids <- taxize_ds$rank_ref[
  	which_rank(downto):NROW(taxize_ds$rank_ref), "rankid"]

	stop_ <- "not"
	notout <- data.frame(rankname = "")
	out <- list()
	if (intermediate) intermed <- list()
	iter <- 0
	while (stop_ == "not") {
		iter <- iter + 1
		if (!nchar(as.character(notout$rankname[[1]])) > 0) {
			temp <- dt2df(lapply(as.character(id), ritis::rank_name),
				idcol = FALSE)
		} else {
		  temp <- notout
		}
		tt <- dt2df(lapply(as.character(temp$tsn), ritis::hierarchy_down),
			idcol = FALSE)
		## we need to run rank_name() to get 'rankid'
		names_ <- dt2df(lapply(split(tt, row.names(tt)), function(x) {
		  ritis::rank_name(as.character(x$tsn))[,c("rankid","rankname","tsn")]
		}), idcol = FALSE)
		##
		if (nrow(names_) == 0) {
		  out[[iter]] <- data.frame(tsn = "No data", parentname = "No data",
		                            parenttsn = "No data", taxonname = "No data",
		                            rankid = "No data", rankname = "No data")
		  stop_ <- "nodata"
		} else {
		  tt <- merge(tt, names_[,-2], by = "tsn")
		  if (intermediate) intermed[[iter]] <- tt
		  if (nrow(tt[tt$rankid == downto2, ]) > 0)
		  	out[[iter]] <- tt[tt$rankid == downto2, ]
		  if (nrow(tt[!tt$rankid == downto2, ]) > 0) {
		    notout <- tt[!tt$rankid %in% torank_ids, ]
		  } else {
		    notout <- data.frame(rankname = downto)
		  }

		  if (length(notout$rankname) > 0)
		  	notout$rankname <- tolower(notout$rankname)
		  if (all(notout$rankname == downto)) {
		    stop_ <- "fam"
		  } else {
		    id <- notout$tsn
		    stop_ <- "not"
		  }
		}
		if (intermediate) {
		  intermed[[iter]] <- stats::setNames(intermed[[iter]],
		  	tolower(names(intermed[[iter]])))
		  intermed[[iter]]$rankname <- tolower(intermed[[iter]]$rankname)
		}
	}
  tmp <- dt2df(out, idcol = FALSE)
  tmp$rankname <- tolower(tmp$rankname)
	if (intermediate) {
	  list(target = stats::setNames(tmp, tolower(names(tmp))),
	  	intermediate = intermed)
  } else {
    stats::setNames(tmp, tolower(names(tmp)))
  }
}
