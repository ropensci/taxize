#' Taxonomic IDs to taxonomic names
#'
#' @export
#' @param id vector of taxonomic IDs (character or numeric)
#' @param db (character) database to query. One or more of `tol`, `itis`,
#' `ncbi`, `worms`, `gbif`, or `bold`. Note that each
#' taxonomic data source has their own  identifiers, so that if you provide
#' the wrong `db` value for the identifier you could get a result,
#' but it will likely be wrong (not what you were expecting). If using ncbi
#' we recommend getting API keys; see [taxize-authentication]
#' @param x Deprecated, see `id`
#' @param ... Further args passed on to `tol_id2name` or
#' [itis_getrecord], or other internal functions.
#' See those functions for what parameters can be passed on.
#'
#' @return A named list of data.frames, named by the input taxonomic ids
#' 
#' @section HTTP version for NCBI requests:
#' We hard code `http_version = 2L` to use HTTP/1.1 in HTTP requests to
#' the Entrez API. See `curl::curl_symbols('CURL_HTTP_VERSION')` 
#'
#' @examples \dontrun{
#' # ITIS
#' id2name(19322, db = "itis")
#'
#' # TOL
#' id2name(515698, db = "tol")
#' # get NCBI ID and pass to classification()
#' x <- id2name(515698, db = "tol")
#' classification(as.uid(x[[1]]$tax_sources_ncbi))
#'
#' # NCBI
#' id2name(315567, db = "ncbi")
#' id2name(3339, db = "ncbi")
#' id2name(9696, db = "ncbi")
#' id2name(c(9695, 9696), db = "ncbi")
#'
#' # WORMS
#' id2name(105706, db = "worms")
#'
#' # GBIF
#' id2name(2441176, db = "gbif")
#'
#' # BOLD
#' id2name(88899, db = "bold")
#' }
id2name <- function(id, db = NULL, x = NULL, ...) UseMethod("id2name")

#' @export
#' @rdname id2name
id2name.default <- function(id, db = NULL, x = NULL, ...) {
  nstop(db)
  pchk(x, "sci")
  if (!is.null(x)) id <- x
  if (!db %in% id2name_sources) {
    stop("'db' must be one of ", paste(id2name_sources, collapse = ", "))
  }
  w <- process_idn_ids(id, db)
  stats::setNames(id2name(w, ...), id)
}

id2name_sources <- c('tol', 'itis', 'ncbi', 'worms', 'gbif', 'bold')

process_idn_ids <- function(input, db) {
  as_fxn <- switch(db, tol = as.tolid, itis = as.tsn, ncbi = as.uid,
    worms = as.wormsid, gbif = as.gbifid,
    bold = as.boldid)
  as_fxn(input, check = FALSE)
}

# TOL
#' @export
#' @rdname id2name
id2name.tolid <- function(id, ...) {
  warn_db(list(...), "tol")
  fun <- function(y) {
    if (is.na(y)) NA_character_ else tol_id2name(as.numeric(y))
  }
  out <- lapply(id, fun, ...)
  names(out) <- id
  class(out) <- 'id2name'
  attr(out, 'db') <- 'tol'
  return(out)
}


# ITIS
itis_id2name <- function(x, ...) {
  z <- itis_getrecord(x, ...)
  if (NROW(z) == 0) return(id2name_blanks$itis)
  data.frame(id = x, name = z$scientificName$combinedName,
    rank = strtrim(z$taxRank$rankName),
    status = z$coreMetadata$taxonUsageRating,
    parent_tsn = z$parentTSN$parentTsn,
    stringsAsFactors = FALSE)
}

#' @export
#' @rdname id2name
id2name.tsn <- function(id, ...) {
  warn_db(list(...), "itis")
  fun <- function(y) {
    if (is.na(y)) NA_character_ else itis_id2name(y, ...)
  }
  out <- lapply(id, fun, ...)
  names(out) <- id
  class(out) <- 'id2name'
  attr(out, 'db') <- 'tsn'
  return(out)
}


# NCBI
ncbi_id2name <- function(x, ...) {
  key <- getkey(NULL, "ENTREZ_KEY")
  cli <- crul::HttpClient$new(url = ncbi_base(), headers = tx_ual,
    opts = list(http_version = 2L, ...))
  args <- tc(list(db = "taxonomy", id = x, api_key = key))
  res <- cli$get("entrez/eutils/esummary.fcgi", query = args)
  res$raise_for_status()
  tt <- xml2::read_xml(res$parse("UTF-8"))
  toget <- c("ScientificName", "Rank", "Status")
  res <- vapply(toget, function(z) {
    xml2::xml_text(xml2::xml_find_first(tt, sprintf("//Item[@Name=\"%s\"]", z)))
  }, "")
  if (all(is.na(res))) return(id2name_blanks$ncbi)
  tmp <- rbind.data.frame(c(x, unname(res)), stringsAsFactors = FALSE)
  names(tmp) <- c("id", tolower(names(res)))
  names(tmp)[2] <- "name"
  return(tmp)
}

#' @export
#' @rdname id2name
id2name.uid <- function(id, ...) {
  warn_db(list(...), "ncbi")
  fun <- function(y, ...) {
    if (is.na(y)) NA_character_ else ncbi_id2name(y, ...)
  }
  out <- lapply(id, fun, ...)
  names(out) <- id
  class(out) <- 'id2name'
  attr(out, 'db') <- 'ncbi'
  return(out)
}


# WORMS
worms_id2name <- function(x, ...) {
  res <- worrms::wm_record(as.numeric(x), ...)
  if (length(res) == 0) return(id2name_blanks$ncbi)
  data.frame(id = x, name = res$scientificname,
    rank = res$rank, status = res$status,
    stringsAsFactors = FALSE)
}

#' @export
#' @rdname id2name
id2name.wormsid <- function(id, ...) {
  warn_db(list(...), "worms")
  fun <- function(y) {
    if (is.na(y)) NA_character_ else worms_id2name(y, ...)
  }
  out <- lapply(id, fun, ...)
  names(out) <- id
  class(out) <- 'id2name'
  attr(out, 'db') <- 'worms'
  return(out)
}


# GBIF
gbif_id2name <- function(x, ...) {
  z <- tryCatch(gbif_name_usage(x, ...), error = function(e) e)
  if (inherits(z, "error") || length(z) == 0) return(id2name_blanks$ncbi)
  data.frame(id = x, name = z$canonicalName,
    rank = tolower(z$rank), status = tolower(z$taxonomicStatus),
    stringsAsFactors = FALSE)
}

#' @export
#' @rdname id2name
id2name.gbifid <- function(id, ...) {
  warn_db(list(...), "gbif")
  fun <- function(y) {
    if (is.na(y)) NA_character_ else gbif_id2name(y, ...)
  }
  out <- lapply(id, fun, ...)
  names(out) <- id
  class(out) <- 'id2name'
  attr(out, 'db') <- 'gbif'
  return(out)
}

# BOLD
bold_id2name <- function(x, ...) {
  z <- bold_search(id = x, ...)
  if ("noresults" %in% names(z)) return(id2name_blanks$bold)
  data.frame(id = x, name = z$taxon,
    rank = tolower(z$tax_rank),
    stringsAsFactors = FALSE)
}

#' @export
#' @rdname id2name
id2name.boldid <- function(id, ...) {
  warn_db(list(...), "bold")
  fun <- function(y) {
    if (is.na(y)) NA_character_ else bold_id2name(y, ...)
  }
  out <- lapply(id, fun, ...)
  names(out) <- id
  class(out) <- 'id2name'
  attr(out, 'db') <- 'bold'
  return(out)
}


######## blank data.frames
id2name_blanks <- list(
  itis = data.frame(
    id = character(0),
    name = character(0),
    rank = character(0),
    status = character(0),
    parent_tsn = character(0),
    stringsAsFactors = FALSE
  ),
  ncbi = data.frame(
    id = character(0),
    name = character(0),
    rank = character(0),
    status = character(0),
    stringsAsFactors = FALSE
  ),
  tol = data.frame(
    id = character(0),
    name = character(0),
    rank = character(0),
    tax_sources_ncbi = character(0),
    tax_sources_gbif = character(0),
    tax_sources_irmng = character(0),
    stringsAsFactors = FALSE
  ),
  bold = data.frame(
    id = character(0),
    name = character(0),
    rank = character(0),
    stringsAsFactors = FALSE
  )
)
