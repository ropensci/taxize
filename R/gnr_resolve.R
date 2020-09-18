#' Resolve names using Global Names Resolver
#'
#' See section **Age of datasets in the Global Names Resolver**
#'
#' @export
#' @param sci character; taxonomic names to be resolved. Doesn't work for
#' vernacular/common names.
#' @param data_source_ids character; IDs to specify what data source
#'     is searched. See [gnr_datasources()].
#' @param resolve_once logical; Find the first available match instead of
#'    matches across all data sources with all possible renderings of a name.
#'    When `TRUE`, response is rapid but incomplete.
#' @param with_context logical; Reduce the likelihood of matches to taxonomic
#'    homonyms. When `TRUE` a common taxonomic context is calculated for
#'    all supplied names from matches in data sources that have classification
#'    tree paths. Names out of determined context are penalized during score
#'    calculation.
#' @param canonical logical; If `FALSE` (default), gives back names with
#'    taxonomic authorities. If `TRUE`, returns canocial names
#'    (without tax. authorities and abbreviations).
#' @param highestscore logical; Return those names with the highest score for
#'    each searched name? Defunct
#' @param best_match_only (logical) If `TRUE`, best match only returned.
#' Default: `FALSE`
#' @param preferred_data_sources (character) A vector of one or more data
#' source IDs.
#' @param with_canonical_ranks (logical) Returns names with infraspecific
#' ranks, if present. If `TRUE`, we force `canonical=TRUE`, otherwise
#' this parameter would have no effect. Default: `FALSE`
#' @param http The HTTP method to use, one of "get" or "post". Default: "get".
#' Use `http="post"` with large queries. Queries with > 300 records
#' use "post" automatically because "get" would fail
#' @param names Deprecated, see `sci`
#' @param ... Curl options passed on to [crul::HttpClient]
#' @param cap_first (logical) For each name, fix so that the first name part is
#' capitalized, while others are not. This web service is sensitive to
#' capitalization, so you'll get different results depending on capitalization.
#' First name capitalized is likely what you'll want and is the default.
#' If `FALSE`, names are not modified. Default: `TRUE`
#' @param fields (character) One of minimal (default) or all. Minimal gives
#' back just four fields, whereas all gives all fields back.
#'
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @return A data.frame with one attribute `not_known`: a character
#' vector of taxa unknown to the Global Names Index. Access like
#' `attr(output, "not_known")`, or `attributes(output)$not_known`.
#'
#' Columns of the output data.frame:
#' * user_supplied_name (character) - the name you passed in to the
#'  `names` parameter, unchanged.
#' * submitted_name (character) - the actual name submitted to the GNR
#'  service
#' * data_source_id (integer/numeric) - data source ID
#' * data_source_title (character) - data source name
#' * gni_uuid (character) - Global Names Index UUID (aka identifier)
#' * matched_name (character) - the matched name in the GNR service
#' * matched_name2 (character) - returned if `canonical=TRUE`, in
#'  which case **matched_name** is not returned
#' * classification_path (character) - names of the taxonomic
#'  classification tree, with names separated by pipes (`|`)
#' * classification_path_ranks (character) - ranks of the taxonomic
#'  classification tree, with names separated by pipes (`|`)
#' * classification_path_ids (character) - identifiers of the taxonomic
#'  classification tree, with names separated by pipes (`|`)
#' * taxon_id (character) - taxon identifier
#' * edit_distance (integer/numeric) - edit distance
#' * imported_at (character) - date imported
#' * match_type (integer/numeric) - match type
#' * match_value (character) - description of match type
#' * prescore (character) - pre score
#' * score (numeric) - score
#' * local_id (character) - local identifier
#' * url (character) - URL for taxon
#' * global_id (character) - global identifier
#' * current_taxon_id (character) - current taxon id
#' * current_name_string (character) - current name string
#'
#' Note that names (i.e. rows) are dropped that are NA, are zero length
#' strings, are not character vectors, or are not found by the API.
#'
#' @section Age of datasets in the Global Names Resolver:
#' IMPORTANT: Datasets used in the Global Names Resolver vary in how recently
#' they've been updated. See the `updated_at` field in the
#' output of [gnr_datasources()] for dates when each dataset
#' was last updated.
#'
#' @section preferred_data_sources:
#' If `preferred_data_sources` is used, only the preferred data
#' is returned - if it has any results.
#'
#' @seealso [gnr_datasources()] [tnrs]
#' @keywords resolve names taxonomy
#' @references http://gnrd.globalnames.org/api
#' http://gnrd.globalnames.org/
#' @examples \dontrun{
#' gnr_resolve(sci = c("Helianthus annuus", "Homo sapiens"))
#' gnr_resolve(sci = c("Asteraceae", "Plantae"))
#'
#' # Using data source 12 (Encyclopedia of Life)
#' sources <- gnr_datasources()
#' sources
#' eol <- sources$id[sources$title == 'EOL']
#' gnr_resolve(names=c("Helianthos annuus","Homo sapians"), data_source_ids=eol)
#'
#' # Two species in the NE Brazil catalogue
#' sps <- c('Justicia brasiliana','Schinopsis brasiliensis')
#' gnr_resolve(sci = sps, data_source_ids = 145)
#'
#' # Best match only, compare the two
#' gnr_resolve(sci = "Helianthus annuus", best_match_only = FALSE)
#' gnr_resolve(sci = "Helianthus annuus", best_match_only = TRUE)
#'
#' # Preferred data source
#' gnr_resolve(sci = "Helianthus annuus", preferred_data_sources = c(3,4))
#'
#' # Return canonical names - default is canonical=FALSE
#' head(gnr_resolve(sci = "Helianthus annuus"))
#' head(gnr_resolve(sci = "Helianthus annuus", canonical=TRUE))
#'
#' # Return canonical names with authority stripped but
#' # ranks still present
#' gnr_resolve("Scorzonera hispanica L. subsp. asphodeloides Wallr.")
#' ## vs.
#' gnr_resolve("Scorzonera hispanica L. subsp. asphodeloides Wallr.",
#'    with_canonical_ranks = TRUE)
#' }

gnr_resolve <- function(sci, data_source_ids = NULL, resolve_once = FALSE,
  with_context = FALSE, canonical = FALSE, highestscore = TRUE,
  best_match_only = FALSE, preferred_data_sources = NULL,
  with_canonical_ranks = FALSE, http = "get", cap_first = TRUE,
  fields = "minimal", names = NULL, ...) {

  pchk(names, "sci")
  if (!is.null(names)) sci <- names
  fields <- match.arg(fields, c("minimal", "all"))
  http <- match.arg(http, c("get", "post"))
  num <- NULL
  url <- "https://resolver.globalnames.org/name_resolvers.json"
  # clean out zero length strings
  sci <- Filter(function(x) nzchar(x) && !is.na(x) && is.character(x), sci)
  # store original sci supplied by user
  orig_names <- sci
  if (cap_first) sci <- taxize_capwords(sci, onlyfirst = TRUE)
  names2 <- paste0(sci, collapse = "|")
  if (length(sci) > 300 && http == "get") http <- "post"

  data_source_ids <- paste0(data_source_ids, collapse = "|")
  preferred_data_sources <- paste0(preferred_data_sources, collapse = "|")
  if (nchar(preferred_data_sources, keepNA = FALSE) == 0)
    preferred_data_sources <- NULL
  if (with_canonical_ranks) canonical <- TRUE

  args <- tc(list(names = names2, data_source_ids = data_source_ids,
            resolve_once = cv(resolve_once), with_context = cv(with_context),
            best_match_only = cv(best_match_only),
            preferred_data_sources = preferred_data_sources,
            with_canonical_ranks = cv(with_canonical_ranks)))
  args <- argsnull(args)

  cli <- crul::HttpClient$new(url = url,
    headers = tx_ual, opts = list(...))
  if (http == 'get') {
    tmp <- cli$get(query = args)
    tmp$raise_for_status()
    tmp2 <- tmp$parse("UTF-8")
    dat <- jsonlite::fromJSON(tmp2, FALSE)$data
  } else {
    args <- args[!names(args) %in% "names"]
    nms <- split(sci, ceiling(seq_along(sci)/500))
    datbits <- list()
    for (i in seq_along(nms)) {
      tt <- data.frame(paste0(seq_along(nms[[i]]), "|", sci))
      file <- tempfile(fileext = ".txt")
      write.table(tt, file = file, row.names = FALSE,
                  col.names = FALSE, quote = FALSE)
      tmp <- cli$post(query = args, body = list(file = crul::upload(file)))
      tmp$raise_for_status()
      ss <- tmp$parse("UTF-8")
      datbits[[i]] <- jsonlite::fromJSON(ss, FALSE)$data
    }

    dat <- do.call("c", datbits)
  }

  # add original name supplied by user
  dat <- Map(function(x,y) c(original_name = y, x), dat, orig_names)

  to_get <- if (is.null(preferred_data_sources)) "results" else "preferred_results"

  data_ <-
    lapply(dat, function(y) {
      if (!is.null(unlist(y[[to_get]]))) {
        res <- lapply(y[[to_get]], function(x) {
          take_fields <- switch(fields,
            minimal = c("name_string", "data_source_title","score",
                        "canonical_form"),
            all = names(x)
          )
          take <- x[take_fields]
          take[sapply(take, is.null)] <- NA
          return(data.frame(take, stringsAsFactors = FALSE))
        })
      } else {
        res <- NULL
      }
      list(y[c("original_name", "supplied_name_string")], res)
    })
  not_known <- Filter(function(x) is.null(x[[2]]), data_)
  not_known <- sapply(not_known, function(x) x[[1]]$original_name)
  data_ <- Filter(function(x) !is.null(x[[2]]), data_)

  # check for empty data object
  drill <- tryCatch(data_[[1]], error = function(e) e)
  to_rename <- c("original_name", "supplied_name_string", "name_string",
                 "canonical_form")
  if (inherits(drill, "simpleError")) {
    out <- data.frame(NULL)
  } else {
    data_2 <- dt2df(lapply(data_, function(x)
      data.frame(x[[1]], dt2df( if (length(x[[2]]) == 0) {
      list(data.frame(name_string = "", data_source_title = "", score = NaN,
                      canonical_form = ""))
    } else {
      x[[2]]
    }, idcol = FALSE), stringsAsFactors = FALSE)), idcol = FALSE)
    names(data_2)[names(data_2) %in% to_rename] <-
      c("user_supplied_name", "submitted_name",
        "matched_name", "matched_name2")
    data_2$matched_name <- as.character(data_2$matched_name)
    data_2$data_source_title <- as.character(data_2$data_source_title)
    data_2$matched_name2 <- as.character(data_2$matched_name2)

    if (canonical) {
      data_2 <- data_2[ , !names(data_2) %in% "matched_name"]
    } else {
      data_2 <- data_2[ , !names(data_2) %in% "matched_name2"]
    }
    # canonical = TRUE, may result into duplicates
    out <- unique(data_2)
  }

  row.names(out) <- NULL
  structure(tibble::as_tibble(out), not_known = not_known)
}

cv <- function(x) {
  if (x) {
    'true'
  } else {
    NULL
  }
}
