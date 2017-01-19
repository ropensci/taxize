#' Use Catalogue of Life to get downstream taxa to a given taxonomic level.
#'
#' @param name The string to search for. Only exact matches found the name given
#'     will be returned, unless one or wildcards are included in the search
#'   	string. An * (asterisk) character denotes a wildcard; a % (percentage)
#'    character may also be used. The name must be at least 3 characters long,
#'    not counting wildcard characters.
#' @param id The record ID of the specific record to return (only for scientific
#'   	names of species or infraspecific taxa)
#' @param downto The taxonomic level you want to go down to. See examples below.
#' 		The taxonomic level IS case sensitive, and you do have to spell it
#' 		correctly. See \code{data(rank_ref)} for spelling.
#' @param format The returned format (default = NULL). If NULL xml is used.
#'    Currently only xml is supported.
#' @param start  The first record to return (default = NULL). If NULL, the
#'    results are returned from the first record (start=0). This is useful if
#'    the total number of results is larger than the maximum number of results
#'    returned by a single Web service query (currently the maximum number of
#'    results returned by a single query is 500 for terse queries and 50 for
#'    full queries).
#' @param checklist The year of the checklist to query, if you want a specific
#' 		year's checklist instead of the lastest as default (numeric).
#' @param verbose Print or suppress messages.
#' @param intermediate (logical) If TRUE, return a list of length two with target
#'    taxon rank names, with additional list of data.frame's of intermediate
#'    taxonomic groups. Default: FALSE
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @details Provide only names instead of id's
#' @return A list of data.frame's.
#' @export
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
#' }

col_downstream <- function(name = NULL, id = NULL, downto, format = NULL,
  start = NULL, checklist = NULL, verbose = TRUE, intermediate = FALSE, ...) {

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
                          format = format, start = start, ...)
        })
      } else {
        tt <- ldply(toget, function(z) {
          search_col_safe(name = z, id = NULL, checklist = checklist,
                          format = format, start = start, ...)
        })
      }

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
  if (verbose)
    message(
      sprintf('These taxa with no data: %s\nTry adjusting input parameters',
              names(nas[nas == 0])))
  return( temp )
}
