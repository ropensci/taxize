#' Resolve names using Global Names Resolver.
#'
#' Uses the Global Names Index, see \url{http://gni.globalnames.org/}.
#'
#' @import stringr plyr httr jsonlite
#' @param names character; taxonomic names to be resolved. Doesn't work for verncular/common names.
#' @param data_source_ids character; IDs to specify what data source
#'     is searched. See \code{\link[taxize]{gnr_datasources}}.
#' @param resolve_once logical; Find the first available match instead of
#'    matches across all data sources with all possible renderings of a name.
#'    When \code{TRUE}, response is rapid but incomplete.
#' @param with_context logical; Reduce the likelihood of matches to taxonomic
#'    homonyms. When \code{TRUE} a common taxonomic context is calculated for
#'    all supplied names from matches in data sources that have classification
#'    tree paths. Names out of determined context are penalized during score
#'    calculation.
#' @param stripauthority logical; If FALSE (default), gives back names with
#'    taxonomic authorities. If TRUE, strips author names.
#' @param highestscore logical; Return those names with the highest score for
#'    each searched name?
#' @param best_match_only (logical) If TRUE, best match only returned.
#' @param preferred_data_sources (character) A vector of one or more data source IDs.
#' @param http The HTTP method to use, one of "get" or "post". Default="get".
#'    Use http="post" with large queries. Queries with > 300 records use "post"
#'    automatically because "get" would fail
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @author Scott Chamberlain {myrmecocystus@@gmail.com}
#' @return A data.frame.
#' @seealso \code{\link[taxize]{gnr_datasources}}
#' @export
#' @keywords resolve names taxonomy
#' @examples \dontrun{
#' gnr_resolve(names = c("Helianthus annuus", "Homo sapiens"))
#' gnr_resolve(names = c("Asteraceae", "Plantae"))
#'
#' # Using data source 12 (Encyclopedia of Life)
#' sources <- gnr_datasources()
#' sources
#' eol <- sources$id[sources$title == 'EOL']
#' gnr_resolve(names=c("Helianthos annuus","Homo sapians"), data_source_ids=eol)
#'
#' # Two species in the NE Brazil catalogue
#' sps <- c('Justicia brasiliana','Schinopsis brasiliensis')
#' gnr_resolve(names = sps, data_source_ids = 145)
#'
#' # Best match only, compare the two
#' gnr_resolve(names = "Helianthus annuus", best_match_only = FALSE)
#' gnr_resolve(names = "Helianthus annuus", best_match_only = TRUE)
#'
#' # Preferred data source
#' gnr_resolve(names = "Helianthus annuus", preferred_data_sources = c(3,4))
#'
#' # Strip taxonomic authorities - default is stripauthority=FALSE
#' head(gnr_resolve(names = "Helianthus annuus")$results)
#' head(gnr_resolve(names = "Helianthus annuus", stripauthority=TRUE)$results)
#' }

gnr_resolve <- function(names, data_source_ids = NULL, resolve_once = FALSE,
  with_context = FALSE, stripauthority = FALSE, highestscore = TRUE, best_match_only = FALSE,
  preferred_data_sources = NULL, http="get", ...) {

  http <- match.arg(http, c("get","post"))
  num = NULL
  url <- "http://resolver.globalnames.org/name_resolvers.json"
  names2 <- paste0(names, collapse = "|")
  if (length(names) > 300 && http == "get") http <- "post"

  data_source_ids <- paste0(data_source_ids, collapse = "|")
  preferred_data_sources <- paste0(preferred_data_sources, collapse = "|")
  if (nchar(preferred_data_sources) == 0) preferred_data_sources <- NULL
  resolve_once <- check_value(resolve_once)
  with_context <- check_value(with_context)
  highestscore <- check_value(highestscore)
  best_match_only <- check_value(best_match_only)

  args <- taxize_compact(list(names = names2, data_source_ids = data_source_ids,
            resolve_once = resolve_once, with_context = with_context,
            best_match_only = best_match_only, preferred_data_sources = preferred_data_sources))
  if (length(args) == 0) args <- NULL

  if (http == 'get') {
    tmp <- GET(url, query = args, ...)
    warn_for_status(tmp)
    tmp2 <- content(tmp, as = "text")
    dat <- jsonlite::fromJSON(tmp2, FALSE)$data
  } else {
    args <- args[!names(args) %in% "names"]
    nms <- split(names, ceiling(seq_along(names)/500))
    datbits <- list()
    for (i in seq_along(nms)) {
      tt <- data.frame(num = 1:length(nms[[i]]), names = nms[[i]])
      tt <- data.frame(ddply(tt, .(num), summarise, paste0(num, "|", names))[,2])
      write.table(tt, file = "~/gnr_names.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)
      ss <- POST(url, query = args, body = list(file = upload_file(path = "~/gnr_names.txt")), ...)
      warn_for_status(ss)
      ss <- content(ss, "text")
      datbits[[i]] <- jsonlite::fromJSON(ss, FALSE)$data
    }

    dat <- do.call("c", datbits)
  }

  data_ <- lapply(dat,
                  function(y)
                    list(y[["supplied_name_string"]],
                        lapply(y$results, function(x) data.frame(x[c("name_string", "data_source_title", "score", "canonical_form")]))))

  drill <- tryCatch(data_[[1]][[2]][[1]], error = function(e) e)
  if (NROW(drill) == 0 || is(drill, "simpleError")) {
    out <- "no results found"
  } else {
    data_2 <- ldply(data_, function(x) data.frame(x[[1]], ldply( if (length(x[[2]]) == 0) {
      list(data.frame(name_string = "", data_source_title = "", score = NaN, canonical_form = ""))
    } else {
      x[[2]]
    }), stringsAsFactors = FALSE))
    names(data_2)[c(1,2,5)] <- c("submitted_name", "matched_name", "matched_name2")
    data_2$matched_name <- as.character(data_2$matched_name)
    data_2$data_source_title <- as.character(data_2$data_source_title)
    data_2$matched_name2 <- as.character(data_2$matched_name2)
    out <- data_2[order(data_2$submitted_name), ]

    if (stripauthority) {
      out <- out[ , !names(out) %in% "matched_name"]
    } else {
      out <- out[ , !names(out) %in% "matched_name2"]
    }
  }

  if (!is.null(preferred_data_sources)) {
    data_preferred <- lapply(dat,
      function(y)
          list(y[["supplied_name_string"]],
              lapply(y$preferred_results, function(x) data.frame(x[c("name_string", "data_source_title", "score", "canonical_form")]))))
    data_2_preferred <- ldply(data_preferred, function(x) data.frame(x[[1]], ldply(if (length(x[[2]]) == 0) {
      list(data.frame(name_string = "", data_source_title = "", score = NaN, canonical_form = ""))
    } else {
      x[[2]]
    }), stringsAsFactors = FALSE))
    names(data_2_preferred)[c(1,2,5)] <- c("submitted_name", "matched_name", "matched_name2")
    data_2_preferred$matched_name <- as.character(data_2_preferred$matched_name)
    data_2_preferred$data_source_title <- as.character(data_2_preferred$data_source_title)
    data_2_preferred$matched_name2 <- as.character(data_2_preferred$matched_name2)
    out_preferred <- data_2_preferred[order(data_2_preferred$submitted_name), ]

    if (stripauthority) {
      out_preferred <- out_preferred[ , !names(out_preferred) %in% "matched_name"]
    } else {
      out_preferred <- out_preferred[ , !names(out_preferred) %in% "matched_name2"]
    }
  } else {
    out_preferred <- NULL
  }

  list(results = out, preferred = out_preferred)
}

check_value <- function(x){
  if (x) {
    'true'
  } else {
    NULL
  }
}
