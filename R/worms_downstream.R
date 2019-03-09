#' Retrieve all taxa names downstream in hierarchy for WORMS
#'
#' @export
#' @param id (integer) One or more AphiaID's
#' @param downto (character) The taxonomic level you want to go down to. 
#' See examples below. The taxonomic level IS case sensitive, and you do have 
#' to spell it correctly. See `data(rank_ref)` for spelling.
#' @param intermediate (logical) If `TRUE`, return a list of length two 
#' with target taxon rank names, with additional list of data.frame's of
#' intermediate taxonomic groups. Default: `FALSE`
#' @param start (integer) Record number to start at
#' @param ... crul options passed on to [`crul::verb-GET`]
#' @return data.frame of taxonomic information downstream to family from e.g.,
#'    Order, Class, etc., or if `intermediated=TRUE`, list of length two,
#'    with target taxon rank names, and intermediate names.
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @examples \dontrun{
#' ## the genus Gadus
#' worms_downstream(id = 125732, downto="species")
#' worms_downstream(id = 125732, downto="species", intermediate=TRUE)
#' 
#' worms_downstream(id = 51, downto="class")
#' worms_downstream(id = 51, downto="subclass", intermediate=TRUE)
#' 
#' worms_downstream(id = 105, downto="subclass")
#' }
worms_downstream <- function(id, downto, intermediate = FALSE, start = 1, 
  ...) {

  should_be('intermediate', intermediate, 'logical')

  downto <- tolower(downto)
  poss_ranks <- unique(do.call(c, sapply(rank_ref$ranks, strsplit, split = ",",
                                         USE.NAMES = FALSE)))
  downto <- match.arg(downto, choices = poss_ranks)
  torank <- sapply(rank_ref[which_rank(downto), "ranks"],
                   function(x) strsplit(x, ",")[[1]][[1]], USE.NAMES = FALSE)

  stop_ <- "not"
  notout <- data.frame(rank = "", stringsAsFactors = FALSE)
  out <- list()
  if (intermediate) intermed <- list()
  iter <- 0
  while (stop_ == "not") {
    iter <- iter + 1
    temp <- ldply(id, worms_info, ...)
    tt <- ldply(temp$AphiaID, function(x) worms_children(x, start = start, 
      ...))
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

  tmp <- ldply(out)
  if (intermediate) {
    list(target = tmp, intermediate = intermed)
  } else {
    tmp
  }
}

worms_info <- function(x, ...) {
  tt <- worrms::wm_record_(id = as.numeric(x), ...)[[1]]
  tt <- tt[sapply(tt, length) != 0]
  data.frame(tt[c('scientificname', 'rank', 'AphiaID')], 
    stringsAsFactors = FALSE)
}

worms_children <- function(x, start = 1, ...) {
  bb <- tryCatch(
    worrms::wm_children(id = as.numeric(x), offset = start, ...),
    error = function(e) e
  )
  if (inherits(bb, "error")) return(data.frame(NULL))
  bb$rank <- tolower(bb$rank)
  names(bb)[names(bb) %in% "AphiaID"] <- "id"
  names(bb)[names(bb) %in% "scientificname"] <- "name"
  bb[c('id', 'name', 'rank')]
}
