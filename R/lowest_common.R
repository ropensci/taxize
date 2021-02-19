#' Retrieve the lowest common taxon and rank for a given taxon name or ID
#'
#' @export
#' @param sci_id Vector of taxa names (character) or id (character or numeric) to
#' query.
#' @param db character; database to query. either `ncbi`, `itis`, `gbif`,
#' `tol`. If using ncbi, we recommend getting an API key;
#' see [taxize-authentication]
#' @param rows (numeric) Any number from 1 to infinity. If the default NA,
#' all rows are considered. Note that this parameter is ignored if you pass in
#' a taxonomic id of any of the acceptable classes: tsn, gbifid, tolid.
#' NCBI has a method for this function but rows doesn't work.
#' @param class_list (list) A list of classifications, as returned from
#' [classification()]
#' @param low_rank (character) taxonomic rank to return, of length 1
#' @param x Deprecated, see `sci_id`
#' @param ... Other arguments passed to [get_tsn()], [get_uid()],
#' [get_gbifid()], [get_tolid()]
#'
#' @return NA when no match, or a data.frame with columns
#' * name
#' * rank
#' * id
#' 
#' @section Authentication:
#' See [taxize-authentication] for help on authentication
#' 
#' @author Jimmy O'Donnell \email{jodonnellbio@@gmail.com}
#' Scott Chamberlain
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
#' # TOL
#' taxa <- c("Angraecum sesquipedale", "Dracula vampira",
#'   "Masdevallia coccinea")
#' (cls <- classification(taxa, db = "tol"))
#' lowest_common(taxa, db = "tol", class_list = cls)
#' lowest_common(get_tolid(taxa), class_list = cls)
#' xx <- get_tolid(taxa)
#' lowest_common(xx, class_list = cls)
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
#' cool_orchid <- c("Angraecum sesquipedale", "Dracula vampira",
#'   "Masdevallia coccinea")
#' orchid_ncbi <- get_uid(cool_orchid)
#' orchid_gbif <- get_gbifid(cool_orchid)
#'
#' cool_orchids2 <- c("Domingoa haematochila", "Gymnadenia conopsea",
#'   "Masdevallia coccinea")
#' orchid_itis <- get_tsn(cool_orchids2)
#'
#' orchid_hier_ncbi <- classification(orchid_ncbi, db = 'ncbi')
#' orchid_hier_gbif <- classification(orchid_gbif, db = 'gbif')
#' orchid_hier_itis <- classification(orchid_itis, db = 'itis')
#'
#' lowest_common(orchid_ncbi, low_rank = 'class')
#' lowest_common(orchid_ncbi, class_list = orchid_hier_ncbi,
#'   low_rank = 'class')
#' lowest_common(orchid_gbif, low_rank = 'class')
#' lowest_common(orchid_gbif, orchid_hier_gbif, low_rank = 'class')
#' lowest_common(get_uid(cool_orchid), low_rank = 'class')
#' lowest_common(get_uid(cool_orchid), low_rank = 'family')
#'
#' lowest_common(orchid_ncbi, class_list = orchid_hier_ncbi,
#'   low_rank = 'subfamily')
#' lowest_common(orchid_gbif, class_list = orchid_hier_gbif,
#'   low_rank = 'subfamily')
#'
#' lowest_common(orchid_itis, class_list = orchid_hier_itis,
#'   low_rank = 'class')
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
#' @method lowest_common default
lowest_common.default <- function(sci_id, db = NULL, rows = NA, class_list = NULL,
                                  low_rank = NULL, x = NULL, ...) {
  if (is.null(db)) if (!is.null(class_list)) db <- attr(class_list, "db")
  nstop(db)
  pchk(x, "sci_id")
  if (!is.null(x)) sci_id <- x
  switch(
    db,
    itis = {
      id <- process_lowest_ids(sci_id, db, get_tsn, rows = rows, ...)
      lowest_common(id, class_list, ...)
    },
    ncbi = {
      id <- process_lowest_ids(sci_id, db, get_uid, rows = rows, ...)
      lowest_common(id, class_list, ...)
    },
    gbif = {
      id <- process_lowest_ids(sci_id, db, get_gbifid, rows = rows, ...)
      lowest_common(id, class_list, ...)
    },
    tol = {
      id <- process_lowest_ids(sci_id, db, get_tolid, rows = rows, ...)
      lowest_common(id, class_list, ...)
    },
    stop("the provided db value was not recognised", call. = FALSE)
  )
}

#' @export
#' @rdname lowest_common
lowest_common.uid <- function(sci_id, class_list = NULL, low_rank = NULL, ...) {
  check_lowest_ids(sci_id)
  class_list <- get_class(sci_id, class_list, db = "uid", ...)
  lc_helper(sci_id, class_list, low_rank, ...)
}

#' @export
#' @rdname lowest_common
lowest_common.tsn <- function(sci_id, class_list = NULL, low_rank = NULL, ...) {
  check_lowest_ids(sci_id)
  class_list <- get_class(sci_id, class_list, db = "itis", ...)
  lc_helper(sci_id, class_list, low_rank, ...)
}

#' @export
#' @rdname lowest_common
lowest_common.gbifid <- function(sci_id, class_list = NULL, low_rank = NULL, ...) {
  check_lowest_ids(sci_id)
  class_list <- get_class(sci_id, class_list, db = "gbif", ...)
  lc_helper(sci_id, class_list, low_rank, ...)
}

#' @export
#' @rdname lowest_common
lowest_common.tolid <- function(sci_id, class_list = NULL, low_rank = NULL, ...) {
  check_lowest_ids(sci_id)
  class_list <- get_class(sci_id, class_list, db = "tol", ...)
  names(class_list) <- sci_id
  lc_helper(sci_id, class_list, low_rank, ...)
}

# helpers -------------------------------------------------
lc_helper <- function(x, class_list, low_rank = NULL, ...) {
  idc <- class_list[x]
  # next line NCBI specific
  cseq <- vapply(idc, function(x) x[1, 1] != "unclassified sequences",
                 logical(1))
  idc <- idc[cseq]
  if (is.null(low_rank)) {
    x_row <- length(Reduce(intersect, lapply(idc, "[[", 1)))
    x <- idc[[1]][x_row, ]
    if (x[1, "rank"] == "no rank") {
      x[1, "rank"] <- next_best_taxon(idc[[1]][1:x_row, ])
    }
    return(x)
  } else {
    valid_ranks <- tolower(ritis::rank_names()$rankname)
    if (!(low_rank %in% valid_ranks)) {
      warning('the supplied rank is not valid')
    }
    # low_rank_names <- as.character(unique(unlist(lapply(idc, function(x) x$name[which(x$rank == low_rank)]))))
    low_rank_names <- unique(setDF(rbindlist(lapply(idc, function(x)
      x[which(x$rank == low_rank),]))))
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
    if (db == "uid") db <- "ncbi"
    classification(x, db = db, ...)
  } else {
    yattr <- sub("ncbi", "uid", attr(y, "db"))
    if (yattr != db) {
      stop(sprintf("class_list input must be of class '%s'", db), call. = FALSE)
    }
    y
  }
}

check_lowest_ids <- function(x) {
  notmiss <- na.omit(x)
  if (length(notmiss) < 2) {
    stop(length(notmiss), " found, ", length(x) - length(notmiss),
         " were NA; > 1 needed", call. = FALSE)
  }
}

process_lowest_ids <- function(input, db, fxn, ...) {
  g <- tryCatch(as.numeric(as.character(input)), warning = function(e) e)
  if (inherits(g, "condition")) eval(fxn)(input, ...)
  if (is.numeric(g) || is.character(input) && all(grepl("[[:digit:]]", input))) {
    as_fxn <- switch(db, itis = as.tsn, gbif = as.gbifid,
      ncbi = as.uid, tol = as.tolid)
    as_fxn(input, check = FALSE)
  } else {
    eval(fxn)(input, ...)
  }
}
