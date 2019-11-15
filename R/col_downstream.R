#' Use Catalogue of Life to get downstream taxa to a given taxonomic level
#'
#' @export
#' @param name The string to search for. Only exact matches found the name given
#' will be returned, unless one or wildcards are included in the search
#' string. An * (asterisk) character denotes a wildcard; a percent
#' character may also be used. The name must be at least 3 characters long,
#' not counting wildcard characters.
#' @param id The record ID of the specific record to return (only for scientific
#' names of species or infraspecific taxa)
#' @param downto The taxonomic level you want to go down to. See examples below.
#' The taxonomic level IS case sensitive, and you do have to spell it
#' correctly. See `data(rank_ref)` for spelling.
#' @param format The returned format (default = NULL). If NULL xml is used.
#' Currently only xml is supported.
#' @param start  The first record to return (default = NULL). If NULL, the
#' results are returned from the first record (start=0). This is useful if
#' the total number of results is larger than the maximum number of results
#' returned by a single Web service query (currently the maximum number of
#' results returned by a single query is 500 for terse queries and 50 for
#' full queries).
#' @param checklist The year of the checklist to query, if you want a specific
#' year's checklist instead of the lastest as default (numeric).
#' @param messages Print or suppress messages.
#' @param intermediate (logical) If `TRUE`, return a list of length two
#' with target taxon rank names, with additional list of data.frame's of
#' intermediate taxonomic groups. Default: `FALSE`
#' @param extant_only (logical) keep extant taxa only? default: `FALSE`.
#' by default we give back all taxa. set to `TRUE` to get only
#' extant taxa
#' @param ... Curl options passed on to [crul::verb-GET]
#' @details Provide only names instead of id's
#' @section Rate limiting:
#' COL introduced rate limiting recently (writing this on 2019-11-14),
#' but we've no information on what the rate limits are. If you do run into
#' this you'll see an error like "Error: Too Many Requests (HTTP 429)",
#' you'll need to time your requests to avoid the rate limiting, for
#' example, by putting `Sys.sleep()` in between simultaneous requests.
#' @return A list of data.frame's, where each data.frame has columns:
#' * childtaxa_id: (character) COL identifier
#' * childtaxa_name: (character) taxonomic name
#' * childtaxa_rank: (character) rank name
#' * childtaxa_extinct: (logical) extinct or not
#'
#' @examples \dontrun{
#' # Some basic examples
#' col_downstream(name="Apis", downto="species")
#' col_downstream(name="Bryophyta", downto="family")
#'
#' # get classes down from the kingdom Animalia
#' col_downstream(name="Animalia", downto="class")
#' col_downstream(name="Animalia", downto="class", intermediate=TRUE)
#'
#' # An example that takes a bit longer
#' col_downstream(name=c("Plantae", "Animalia"), downto="class")
#'
#' # Using a checklist from a specific year
#' col_downstream(name="Bryophyta", downto="family", checklist=2009)
#'
#' # By id
#' col_downstream(id='576d098d770a39d09e2bcfa1c0896b26', downto="species",
#'   checklist=2012)
#'
#' # keep extant taxa only, prunes out extinct taxa
#' col_downstream(name = "Insecta", downto = "order")
#' col_downstream(name = "Insecta", downto = "order", extant_only = TRUE)
#' }

col_downstream <- function(name = NULL, id = NULL, downto, format = NULL,
  start = NULL, checklist = NULL, messages = TRUE, intermediate = FALSE,
  extant_only = FALSE, ...) {

  downto <- tolower(downto)
  poss_ranks <- unique(do.call(c, sapply(rank_ref$ranks, strsplit, split = ",",
                                         USE.NAMES = FALSE)))
  downto <- match.arg(downto, choices = poss_ranks)

  func <- function(x=NULL, y=NULL, checklist, format, start, ...) {
    url <- make_url(checklist)
    torank <- sapply(rank_ref[which_rank(downto), "ranks"],
                function(x) strsplit(x, ",")[[1]][[1]], USE.NAMES = FALSE)

    toget <- ifelse(is.null(y), x, y)
    stop_ <- "not"
    notout <- data.frame(rankName = "")
    out <- list()
    if (intermediate) intermed <- list()
    iter <- 0
    while (stop_ == "not") {
      iter <- iter + 1
      if (is.null(x)) {
        tt <- ldply(toget, function(z) {
          search_col_safe(name = NULL, id = z, checklist = checklist,
                          format = format, start = start,
                          extant_only = extant_only, ...)
        })
      } else {
        tt <- ldply(toget, function(z) {
          search_col_safe(name = z, id = NULL, checklist = checklist,
                          format = format, start = start,
                          extant_only = extant_only, ...)
        })
      }
      # prune if too low
      tt <- prune_too_low_col(tt, downto)

      if (NROW(tt) == 0) {
        out[[iter]] <- data.frame(stringsAsFactors = FALSE)
        stop_ <- "nodata"
      } else {
        # remove
        if (NROW(tt[tt$childtaxa_rank == downto, ]) > 0) {
          out[[iter]] <- tt[tt$childtaxa_rank == downto, ]
        }

        if (NROW(tt[!tt$childtaxa_rank == downto, ]) > 0) {
          notout <- tt[!tt$childtaxa_rank %in% torank, ]
        } else {
          notout <- data.frame(rankName = downto)
        }

        if (all(notout$childtaxa_rank == downto)) {
          stop_ <- "fam"
        } else {
          if (intermediate) intermed[[iter]] <- notout
          x <- NULL
          toget <- as.character(notout$childtaxa_id)
          stop_ <- "not"
        }
      }
    } # end while loop

    if (length(out) == 0) {
      ret <-  data.frame(childtaxa_id = NA, childtaxa_name = NA,
                         childtaxa_rank = NA)
    } else {
      res <- tc(out)
      ret <- do.call(rbind.fill, res)
    }
    if (intermediate) list(target = ret, intermediate = intermed) else ret
  } # end fxn func

  safe_func <- plyr::failwith(NULL, func)
  if (is.null(id)) {
    temp <- setNames(lapply(name, safe_func, y = NULL,
                            checklist = checklist, format = format,
                            start = start, ...), name)
  } else {
    temp <- setNames(lapply(id, function(z) {
      safe_func(x = NULL, y = id, checklist = checklist, format = format,
                start = start, ...)
    }), id)
  }

  nas <- sapply(temp, function(z)
    NROW(na.omit( if (intermediate) z$target else z )))
  if (messages)
    message(
      sprintf('These taxa with no data: %s\nTry adjusting input parameters',
              names(nas[nas == 0])))
  return( temp )
}
