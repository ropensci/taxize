#' Retrieve all taxa names downstream in hierarchy for BOLD
#'
#' @export
#' @param id (integer) One or more BOLD taxonomic identifiers
#' @param downto (character) The taxonomic level you want to go down to. 
#' See examples below. The taxonomic level IS case sensitive, and you do have 
#' to spell it correctly. See `data(rank_ref)` for spelling.
#' @param intermediate (logical) If `TRUE`, return a list of length two 
#' with target taxon rank names, with additional list of data.frame's of
#' intermediate taxonomic groups. Default: `FALSE`
#' @param ... crul options passed on to [crul::verb-GET]
#' @return data.frame of taxonomic information downstream to family from e.g.,
#' Order, Class, etc., or if `intermediated=TRUE`, list of length two,
#' with target taxon rank names, and intermediate names.
#' @details
#' BEWARE: This function scrapes the BOLD website, so may be unstable. That is,
#' one day it may work, and the next it may fail. Open an issue if you
#' encounter an error: https://github.com/ropensci/taxize/issues
#' @examples \dontrun{
#' ## the genus Gadus
#' bold_downstream(id = 3451, downto="species")
#' 
#' bold_downstream(id = 443, downto="genus")
#' bold_downstream(id = 443, downto="genus", intermediate=TRUE)
#' }
bold_downstream <- function(id, downto, intermediate = FALSE, ...) {
  should_be('intermediate', intermediate, 'logical')

  downto <- tolower(downto)
  poss_ranks <- unique(do.call(c,
    sapply(taxize_ds$rank_ref$ranks, strsplit, split = ",",
      USE.NAMES = FALSE)))
  downto <- match.arg(downto, choices = poss_ranks)
  torank <- sapply(taxize_ds$rank_ref[which_rank(downto), "ranks"],
    function(x) strsplit(x, ",")[[1]][[1]], USE.NAMES = FALSE)

  stop_ <- "not"
  notout <- data.frame(rank = "", stringsAsFactors = FALSE)
  out <- list()
  if (intermediate) intermed <- list()
  iter <- 0
  while (stop_ == "not") {
    iter <- iter + 1
    tt <- dt2df(unlist(lapply(id, bold_children), FALSE), idcol = FALSE)
    tt <- prune_too_low(tt, downto)

    if (NROW(tt) == 0) {
      out[[iter]] <- data.frame(stringsAsFactors = FALSE)
      stop_ <- "nodata"
    } else {
      if (intermediate) intermed[[iter]] <- tt
      if (NROW(tt[tt$rank == downto, ]) > 0) {
        out[[iter]] <- tt[tt$rank == downto, ]
      }
      if (NROW(tt[!tt$rank == downto, ]) > 0) {
        notout <- tt[!tt$rank %in% torank, ]
      } else {
        notout <- data.frame(rank = downto, stringsAsFactors = FALSE)
      }

      if (all(notout$rank == downto)) {
        stop_ <- "fam"
      } else {
        id <- notout$id
        stop_ <- "not"
      }
    }
  } # end while loop

  tmp <- dt2df(out, idcol = FALSE)
  tmp <- tibble::as_tibble(tmp)
  if (intermediate) {
    intermed <- lapply(intermed, tibble::as_tibble)
    list(target = tmp, intermediate = intermed)
  } else {
    tmp
  }
}
