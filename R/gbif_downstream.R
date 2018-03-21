#' Retrieve all taxa names downstream in hierarchy for GBIF
#'
#' @export
#' @param key A taxonomic serial number.
#' @param downto The taxonomic level you want to go down to. See examples below.
#' 		The taxonomic level IS case sensitive, and you do have to spell it
#' 		correctly. See \code{data(rank_ref)} for spelling.
#' @param intermediate (logical) If TRUE, return a list of length two with
#' target taxon rank names, with additional list of data.frame's of
#' intermediate taxonomic groups. Default: FALSE
#' @param limit Number of records to return
#' @param start Record number to start at
#' @param ... Further args passed on to \code{\link{gbif_name_usage}}
#' @return data.frame of taxonomic information downstream to family from e.g.,
#' 		Order, Class, etc., or if \code{intermediated=TRUE}, list of length two,
#'   	with target taxon rank names, and intermediate names.
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @details Sometimes records don't have a \code{canonicalName} entry which is 
#' what we look for. In that case we grab the \code{scientificName} entry. 
#' You can see the type of name colleceted in the column \code{name_type}
#' @examples \dontrun{
#' ## the plant class Bangiophyceae
#' gbif_downstream(key = 198, downto="genus")
#' gbif_downstream(key = 198, downto="genus", intermediate=TRUE)
#'
#' # families downstream from the family Strepsiptera (twisted wing parasites)
#' gbif_downstream(key = 1227, "family")
#' ## here, intermediate leads to the same result as the target
#' gbif_downstream(key = 1227, "family", intermediate=TRUE)
#'
#' # Lepidoptera
#' gbif_downstream(key = 797, "family")
#'
#' # get species downstream from the genus Ursus
#' gbif_downstream(key = 2433406, "species")
#'
#' # get tribes down from the family Apidae
#' gbif_downstream(key = 7799978, downto="species")
#' gbif_downstream(key = 7799978, downto="species", intermediate=TRUE)
#' 
#' # names that don't have canonicalname entries for some results
#' key <- get_gbifid("Myosotis")
#' res <- gbif_downstream(key, downto = "species")
#' res2 <- downstream(key, db = "gbif", downto = "species")
#' }

gbif_downstream <- function(key, downto, intermediate = FALSE, limit = 100,
  start = NULL, ...) {

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
    temp <- ldply(key, function(x) gbif_name_usage_clean(x))
    tt <- ldply(temp$key, function(x) gbif_name_usage_children(x,
      limit = limit, start = start, ...))
    tt <- prune_too_low(tt, downto)

    if (NROW(tt) == 0) {
      out[[iter]] <- data.frame(stringsAsFactors = FALSE)
      stop_ <- "nodata"
    } else {
      if (intermediate) intermed[[iter]] <- tt
      if (NROW(tt[tt$rank == downto, ]) > 0)
        out[[iter]] <- tt[tt$rank == downto, ]
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
  tt$rank <- tolower(tt$rank)
  tt <- setNames(tt, tolower(names(tt)))
  data.frame(tt, stringsAsFactors = FALSE)[, c('canonicalname', 'rank', 'key')]
}

gbif_name_usage_children <- function(x, limit = 100, start = NULL, ...) {
  tt <- gbif_name_usage(x, data = 'children', limit = limit, start = start, ...)$results
  rbind.fill(lapply(tt, function(z) {
    z <- z[sapply(z, length) != 0]
    df <- data.frame(z, stringsAsFactors = FALSE)
    df$rank <- tolower(df$rank)
    df <- stats::setNames(df, tolower(names(df)))
    nms <- c('rank', 'key')
    if ('canonicalname' %in% names(df)) {
      nms <- c('canonicalname', nms) 
      type <- "canonicalname"
    } else {
      nms <- c('scientificname', nms)
      type <- "scientificname"
    }
    dd <- df[, nms]
    dd <- stats::setNames(dd, c('name', 'rank', 'key'))
    dd$name_type <- type
    dd
  }))
}
