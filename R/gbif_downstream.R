#' Retrieve all taxonomic names downstream in hierarchy for GBIF
#'
#' @export
#' @param id A taxonomic serial number.
#' @param downto The taxonomic level you want to go down to. See examples below.
#' 		The taxonomic level IS case sensitive, and you do have to spell it
#' 		correctly. See `data(rank_ref)` for spelling.
#' @param intermediate (logical) If TRUE, return a list of length two with
#' target taxon rank names, with additional list of data.frame's of
#' intermediate taxonomic groups. Default: FALSE
#' @param limit Number of records to return. default: 100. max: 1000. use in
#' combination with the `start` parameter
#' @param start Record number to start at. default: 0. use in combination 
#' with the `limit` parameter
#' @param key Deprecated, see `id`
#' @param ... Further args passed on to [gbif_name_usage()]
#' @return data.frame of taxonomic information downstream to family from e.g.,
#' 		Order, Class, etc., or if `intermediated=TRUE`, list of length two,
#'   	with target taxon rank names, and intermediate names.
#' @author Scott Chamberlain
#' @details Sometimes records don't have a `canonicalName` entry which is 
#' what we look for. In that case we grab the `scientificName` entry. 
#' You can see the type of name colleceted in the column `name_type`
#' @examples \dontrun{
#' ## the plant class Bangiophyceae
#' gbif_downstream(id = 198, downto="genus")
#' gbif_downstream(id = 198, downto="genus", intermediate=TRUE)
#'
#' # families downstream from the family Strepsiptera (twisted wing parasites)
#' gbif_downstream(id = 1227, "family")
#' ## here, intermediate leads to the same result as the target
#' gbif_downstream(id = 1227, "family", intermediate=TRUE)
#' 
#' if (interactive()) {
#' # Lepidoptera
#' gbif_downstream(id = 797, "family")
#'
#' # get species downstream from the genus Ursus
#' gbif_downstream(id = 2433406, "species")
#'
#' # get tribes down from the family Apidae
#' gbif_downstream(id = 7799978, downto="species")
#' gbif_downstream(id = 7799978, downto="species", intermediate=TRUE)
#' 
#' # names that don't have canonicalname entries for some results
#' # Myosotis: key 2925668
#' key <- 2925668
#' res <- gbif_downstream(key, downto = "species")
#' res2 <- downstream(key, db = "gbif", downto = "species")
#' 
#' # Pagination
#' # GBIF limits queries to a maximum of 1000 records per request, so if
#' # there's more than 1000, use the start parameter
#' # Piper, taxonKey = 3075433
#' x1 <- gbif_downstream(id = 3075433, downto = "species", limit=1000)
#' x2 <- gbif_downstream(id = 3075433, downto = "species", limit=1000,
#'   start=1000)
#' x3 <- gbif_downstream(id = 3075433, downto = "species", limit=1000,
#'   start=2000)
#' x4 <- gbif_downstream(id = 3075433, downto = "species", limit=1000,
#'   start=3000)
#' rbind(x1, x2, x3, x4)
#' }
#' }

gbif_downstream <- function(id, downto, intermediate = FALSE, limit = 100,
  start = NULL, key = NULL, ...) {

  if (!is.null(key)) {
    lifecycle::deprecate_warn(when = "v0.9.97", what = "gbif_downstream(key)", with = "gbif_downstream(id)")
    id <- key
  }
  
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
    temp <- dt2df(lapply(id, function(x) gbif_name_usage_clean(x)),
      idcol = FALSE)
    tt <- dt2df(lapply(temp$key, function(x) gbif_name_usage_children(x,
      limit = limit, start = start, ...)), idcol = FALSE)
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
        id <- notout$key
        stop_ <- "not"
      }
    }
    if (intermediate) intermed[[iter]] <- intermed[[iter]]
  } # end while loop

  tmp <- dt2df(out, idcol = FALSE)
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
  dt2df(lapply(tt, function(z) {
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
  }), idcol = FALSE)
}
