#' Retrieve all taxa names downstream in hierarchy for GBIF
#'
#' @export
#' @param key A taxonomic serial number.
#' @param downto The taxonomic level you want to go down to. See examples below.
#' 		The taxonomic level IS case sensitive, and you do have to spell it
#' 		correctly. See \code{data(rank_ref)} for spelling.
#' @param intermediate (logical) If TRUE, return a list of length two with target
#'    taxon rank names, with additional list of data.frame's of intermediate
#'    taxonomic groups. Default: FALSE
#' @param ... Further args passed on to \code{\link{gbif_name_usage}}
#' @return Data.frame of taxonomic information downstream to family from e.g.,
#' 		Order, Class, etc., or if \code{intermediated=TRUE}, list of length two,
#'   	with target taxon rank names, and intermediate names.
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @examples \dontrun{
#' ## the plant class Bangiophyceae
#' gbif_downstream(key = 198, downto="Genus")
#' gbif_downstream(key = 198, downto="Genus", intermediate=TRUE)
#'
#' # get families downstream from the superfamily Acridoidea
#' gbif_downstream(key = 110610447, "Family")
#' ## here, intermediate leads to the same result as the target
#' gbif_downstream(key = 110610447, "Family", intermediate=TRUE)
#'
#' # get species downstream from the genus Ursus
#' gbif_downstream(key = 2433406, "Species")
#'
#' # get tribes down from the family Apidae
#' gbif_downstream(key = 1334757, downto="Species")
#' gbif_downstream(key = 1334757, downto="Species", intermediate=TRUE)
#' }

gbif_downstream <- function(key, downto, intermediate = FALSE, ...) {

  should_be('intermediate', intermediate, 'logical')

  downto <- taxize_capwords(downto)
  poss_ranks <- unique(do.call(c, sapply(rank_ref$ranks, strsplit, split = ",", USE.NAMES = FALSE)))
  downto <- match.arg(downto, choices = poss_ranks)
  torank <- sapply(rank_ref[grep(downto, rank_ref$ranks), "ranks"],
                   function(x) strsplit(x, ",")[[1]][[1]], USE.NAMES = FALSE)

  stop_ <- "not"
  notout <- data.frame(rank = "", stringsAsFactors = FALSE)
  out <- list()
  if (intermediate) intermed <- list()
  iter <- 0
  while (stop_ == "not") {
    iter <- iter + 1
    # if (!nchar(notout$rank[[1]]) > 0) {
    temp <- ldply(key, function(x) gbif_name_usage_clean(x, ...))
    # } else {
    #   temp <- notout
    # }
    tt <- ldply(temp$key, function(x) gbif_name_usage_children(x, ...))

    if (NROW(tt) == 0) {
      out[[iter]] <- data.frame(stringsAsFactors = FALSE)
      stop_ <- "nodata"
    } else {
      if (intermediate) intermed[[iter]] <- tt
      if (NROW(tt[tt$rank == downto, ]) > 0) out[[iter]] <- tt[tt$rank == downto, ]
      if (NROW(tt[!tt$rank == downto, ]) > 0) {
        notout <- tt[!tt$rank %in% torank, ]
      } else {
        notout <- data.frame(rank = downto, stringsAsFactors = FALSE)
      }

      if (all(notout$rank == downto)) {
        stop_ <- "fam"
      } else {
        key <- notout$key
        stop_ <- "not"
      }
    }
    if (intermediate) intermed[[iter]] <- intermed[[iter]]
  } # end while loop

  tmp <- ldply(out)
  if (intermediate) {
    list(target = tmp, intermediate = intermed)
  } else {
    tmp
  }
}

gbif_name_usage_clean <- function(x, ...) {
  tt <- gbif_name_usage(x, ...)
  tt <- tt[sapply(tt, length) != 0]
  tt$rank <- taxize_capwords(tt$rank, onlyfirst = TRUE)
  tt <- setNames(tt, tolower(names(tt)))
  data.frame(tt, stringsAsFactors = FALSE)[, c('canonicalname', 'rank', 'key')]
}

gbif_name_usage_children <- function(x, ...) {
  tt <- gbif_name_usage(x, data = 'children', limit = 100, ...)$results
  rbind.fill(lapply(tt, function(z) {
    z <- z[sapply(z, length) != 0]
    df <- data.frame(z, stringsAsFactors = FALSE)
    df$rank <- taxize_capwords(df$rank, onlyfirst = TRUE)
    df <- setNames(df, tolower(names(df)))
    df[, c('canonicalname', 'rank', 'key')]
  }))
}
