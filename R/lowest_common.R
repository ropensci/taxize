#' Retrieve the lowest common taxon and rank for a given taxon name or ID
#'
#' @export
#' @param x Vector of taxa names (character) or id (character or numeric) to query.
#' @param db character; database to query. either \code{ncbi}, \code{itis}, or
#'    \code{gbif}.
#' @param rows (numeric) Any number from 1 to infinity. If the default NA, all rows are
#' considered. Note that this parameter is ignored if you pass in a taxonomic id of any of the
#' acceptable classes: tsn, colid. NCBI has a method for this function but rows doesn't work.
#' @param class_list (list) A list of classifications, as returned from
#' \code{\link[taxize]{classification}}
#' @param low_rank (character) taxonomic rank to return, of length 1
#' @param ... Other arguments passed to \code{\link[taxize]{get_tsn}},
#'    \code{\link[taxize]{get_uid}}, \code{\link[taxize]{get_eolid}},
#'    \code{\link[taxize]{get_colid}}, \code{\link[taxize]{get_tpsid}},
#'    \code{\link[taxize]{get_gbifid}}.
#'
#' @return NA when no match, or a data.frame with columns
#' \itemize{
#'  \item name
#'  \item rank
#'  \item id
#' }
#' @author Jimmy O'Donnell \email{jodonnellbio@@gmail.com}
#' Scott Chamberlain \email{myrmecocystus@gmail.com}
#' @examples \dontrun{
#' id <- c("9031", "9823", "9606", "9470")
#' id_class <- classification(id, db = 'ncbi')
#' lowest_common(id[2:4], db = "ncbi")
#' lowest_common(id[2:4], db = "ncbi", low_rank = 'class')
#' lowest_common(id[2:4], db = "ncbi", low_rank = 'family')
#' lowest_common(id[2:4], class_list = id_class)
#' lowest_common(id[2:4], class_list = id_class, low_rank = 'class')
#' lowest_common(id[2:4], class_list = id_class, low_rank = 'family')
#'
#' spp <- c("Sus scrofa", "Homo sapiens", "Nycticebus coucang")
#' lowest_common(spp, db = "ncbi")
#' lowest_common(get_uid(spp))
#'
#' lowest_common(spp, db = "itis")
#' lowest_common(get_tsn(spp))
#'
#' gbifid <- c("2704179", "3119195")
#' lowest_common(gbifid, db = "gbif")
#'
#' spp <- c("Poa annua", "Helianthus annuus")
#' lowest_common(spp, db = "gbif")
#' lowest_common(get_gbifid(spp))
#'
#' cool_orchid <- c("Angraecum sesquipedale", "Dracula vampira", "Masdevallia coccinea")
#' orchid_ncbi <- get_uid(cool_orchid)
#' orchid_gbif <- get_gbifid(cool_orchid)
#' orchid_itis <- get_tsn(cool_orchid)
#'
#' orchid_hier_ncbi <- classification(orchid_ncbi, db = 'ncbi')
#' orchid_hier_gbif <- classification(orchid_gbif, db = 'gbif')
#' orchid_hier_itis <- classification(orchid_itis, db = 'itis')
#'
#' lowest_common(orchid_ncbi, low_rank = 'class')
#' lowest_common(orchid_ncbi, class_list = orchid_hier_ncbi, low_rank = 'class')
#' lowest_common(orchid_gbif, low_rank = 'class')
#' lowest_common(orchid_gbif, orchid_hier_gbif, low_rank = 'class')
#' lowest_common(get_uid(cool_orchid), low_rank = 'class')
#' lowest_common(get_uid(cool_orchid), low_rank = 'family')
#'
#' lowest_common(orchid_ncbi, class_list = orchid_hier_ncbi, low_rank = 'subfamily')
#' lowest_common(orchid_gbif, class_list = orchid_hier_gbif, low_rank = 'subfamily')
#'
#' ## Pass in sci. names
#' nms <- c("Angraecum sesquipedale", "Dracula vampira", "Masdevallia coccinea")
#' lowest_common(x = nms, db = "ncbi")
#' lowest_common(x = nms, db = "gbif")
#' # lowest_common(x = nms, db = "itis")
#'
#' ## NAs due to taxon not found, stops with error message
#' # lowest_common(orchid_itis, db = "itis")
#' # lowest_common(get_tsn(cool_orchid))
#' }
lowest_common <- function(...){
  UseMethod("lowest_common")
}

#' @export
#' @rdname lowest_common
lowest_common.default <- function(x, db = NULL, rows = NA, class_list = NULL, low_rank = NULL, ...) {
  if (is.null(db)) if (!is.null(class_list)) db <- attr(class_list, "db")
  nstop(db)
  switch(db,
         itis = {
           id <- process_lowest_ids(x, db, get_tsn, rows = rows, ...)
           lowest_common(id, class_list, ...)
         },
         ncbi = {
           id <- process_lowest_ids(x, db, get_uid, rows = rows, ...)
           lowest_common(id, class_list, ...)
         },
         gbif = {
           id <- process_lowest_ids(x, db, get_gbifid, rows = rows, ...)
           lowest_common(id, class_list, ...)
         },
         stop("the provided db value was not recognised", call. = FALSE)
  )
}

#' @export
#' @rdname lowest_common
lowest_common.uid <- function(x, class_list = NULL, low_rank = NULL, ...) {
  check_lowest_ids(x)
  class_list <- get_class(x, class_list, db = "uid", ...)
  lc_helper(x, class_list, low_rank, ...)
}

#' @export
#' @rdname lowest_common
lowest_common.tsn <- function(x, class_list = NULL, low_rank = NULL, ...) {
  check_lowest_ids(x)
  class_list <- get_class(x, class_list, db = "itis", ...)
  lc_helper(x, class_list, low_rank, ...)
}

#' @export
#' @rdname lowest_common
lowest_common.gbifid <- function(x, class_list = NULL, low_rank = NULL, ...) {
  check_lowest_ids(x)
  class_list <- get_class(x, class_list, db = "gbif", ...)
  lc_helper(x, class_list, low_rank, ...)
}

# helpers -------------------------------------------------
lc_helper <- function(x, class_list, low_rank = NULL, ...) {
  idc <- class_list[x]
  # next line NCBI specific
  cseq <- vapply(idc, function(x) x[1, 1] != "unclassified sequences", logical(1))
  idc <- idc[cseq]
  if (is.null(low_rank)) {
    x_row <- length(Reduce(intersect, lapply(idc, "[[", 1)))
    x <- idc[[1]][x_row, ]
    if (x[1, "rank"] == "no rank") {
      x[1, "rank"] <- next_best_taxon(idc[[1]][1:x_row, ])
    }
    return(x)
  } else {
    valid_ranks <- tolower(getranknames()[,"rankname"])
    if (!(low_rank %in% valid_ranks)) {
      warning('the supplied rank is not valid')
    }
    # low_rank_names <- as.character(unique(unlist(lapply(idc, function(x) x$name[which(x$rank == low_rank)]))))
    low_rank_names <- unique(setDF(rbindlist(lapply(idc, function(x) x[which(x$rank == low_rank),]))))
    if (NROW(low_rank_names) == 1) {
      return(low_rank_names)
    } else {
      return(NA)
    }
  }
}

next_best_taxon <- function(x){
  paste("below-",
        tail(x[, "rank"][!duplicated(x[, "rank"])], n = 1
        ), sep = "")
}

get_class <- function(x, y, db, ...) {
  if (is.null(y)) {
    classification(x, db = db, ...)
  } else {
    yattr <- str_replace(attr(y, "db"), "ncbi", "uid")
    if (yattr != db) {
      stop(sprintf("class_list input must be of class '%s'", db), call. = FALSE)
    }
    y
  }
}

check_lowest_ids <- function(x) {
  notmiss <- na.omit(x)
  if (length(notmiss) < 2) {
    stop(length(notmiss), " found, ", length(x) - length(notmiss), " were NA; > 1 needed", call. = FALSE)
  }
}

process_lowest_ids <- function(input, db, fxn, ...){
  g <- tryCatch(as.numeric(as.character(input)), warning = function(e) e)
  if (is(g, "numeric") || is.character(input) && grepl("[[:digit:]]", input)) {
    as_fxn <- switch(db, itis = as.tsn, gbif = as.gbifid, ncbi = as.uid)
    as_fxn(input, check = FALSE)
  } else {
    eval(fxn)(input, ...)
  }
}
