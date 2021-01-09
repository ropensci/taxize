#' Get IDs from the Catalogue of Life
#'
#' @export
#' @param sci (character) one or more scientific names. Or, a `taxon_state`
#' object (see [taxon-state])
#' @param ask logical; should get_col be run in interactive mode?
#' If TRUE and more than one ID is found for the species, the user is
#' asked for input. If `FALSE` NA is returned for multiple matches.
#' @param messages logical; If TRUE the actual taxon queried is printed on the
#' console.
#' @param rows numeric; Any number from 1 to infinity. If the default NA, all
#' rows are considered. Note that this function still only gives back a col
#' class object with one to many identifiers. See `get_col_()` to get back all,
#' or a subset, of the raw data that you are presented during the ask process.
#' @param minRank,maxRank (character) filter by rank. See [rcol::cp_nu_search()]
#' for options
#' @param status (character) filter by taxonomic status. one of: accepted,
#' doubtful, ambiguous synonym
#' @param fuzzy (logical) `TRUE` or `FALSE`. default: `NULL`
#' @param x Input to [as.col()]
#' @param limit (numeric) number of records to return, applies to `get_col_`
#' only
#' @param check logical; Check if ID matches any existing on the DB, only used
#' in `as.col()`
#' @param ... Ignored
#' @template getreturn
#' @family taxonomic-ids
#' @seealso [classification()]
#' @details Internally we use [rcol::cp_nu_search()]
#' @examples \dontrun{
#' get_col(sci='Poa annua')
#' get_col(sci='Pinus contorta')
#' get_col(sci='Puma concolor')
#' 
#' #lots of queries
#' spp <- names_list("species", 10)
#' res <- get_col(spp)
#' res
#' xx <- taxon_last()
#' xx
#'
#' # multiple names
#' get_col(c("Poa annua", "Pinus contorta"))
#'
#' # specify rows to limit choices available
#' get_col(sci='Satyrium', status = NULL)
#' get_col(sci='Satyrium', rows=10)
#' get_col(sci='Satyrium', rows=1:3)
#'
#' # When not found, NA given
#' get_col(sci="uaudnadndj")
#' get_col(c("Chironomus riparius", "uaudnadndj"))
#'
#' # Narrow down results to a division or rank, or both
#' ## Satyrium example
#' ### Results w/o narrowing
#' get_col("Satyrium")
#' ### w/ phylum
#' get_col("Satyrium", phylum = "Tracheophyta")
#' get_col("Satyrium", phylum = "Arthropoda")
#' ### w/ phylum & rank
#' get_col("Satyrium", phylum = "Arthropoda", rank = "genus")
#'
#' ## min/max rank example
#' get_col("Poa", rank = "genus")
#' get_col("Poa", family = "Thripidae")
#'
#' # Fuzzy filter
#' get_col("A*", fuzzy = FALSE)
#' get_col("A*", fuzzy = TRUE)
#'
#' # Convert a uid without class information to a uid class
#' as.col(get_col("Poa annua")) # already a uid, returns the same
#' as.col(get_col(c("Poa annua","Puma concolor"))) # same
#' as.col(2704179) # numeric
#' as.col(c(2704179,2435099,3171445)) # numeric vector, length > 1
#' as.col("2704179") # character
#' as.col(c("2704179","2435099","3171445")) # character vector, length > 1
#' as.col(list("2704179","2435099","3171445")) # list, either numeric or character
#' ## dont check, much faster
#' as.col("2704179", check=FALSE)
#' as.col(2704179, check=FALSE)
#' as.col(2704179, check=FALSE)
#' as.col(c("2704179","2435099","3171445"), check=FALSE)
#' as.col(list("2704179","2435099","3171445"), check=FALSE)
#'
#' (out <- as.col(c(2704179,2435099,3171445)))
#' data.frame(out)
#' as.uid( data.frame(out) )
#'
#' # Get all data back
#' get_col_("Puma concolor")
#' get_col_(c("Pinus", "uaudnadndj"))
#' get_col_(c("Pinus", "Puma"), rows=5)
#' get_col_(c("Pinus", "Puma"), rows=1:5)
#'
#' # use curl options
#' invisible(get_col("Quercus douglasii", verbose = TRUE))
#' }
get_col <- function(sci, ask = TRUE, messages = TRUE, rows = NA,
  status = "accepted", minRank = NULL, maxRank = NULL, fuzzy = FALSE, ...) {

  assert(sci, c("character", "taxon_state"))
  assert(ask, "logical")
  assert(messages, "logical")
  assert(status, "character")
  assert(minRank, "character")
  assert(maxRank, "character")
  assert(fuzzy, "logical")
  assert_rows(rows)

  if (inherits(sci, "character")) {
    tstate <- taxon_state$new(class = "col", names = sci)
    items <- sci
  } else {
    assert_state(sci, "col")
    tstate <- sci
    sci <- tstate$taxa_remaining()
    items <- c(sci, tstate$taxa_completed())
  }

  prog <- progressor$new(items = items, suppress = !messages)
  done <- tstate$get()
  for (i in seq_along(done)) prog$completed(names(done)[i], done[[i]]$att)
  prog$prog_start()

  for (i in seq_along(sci)) {
    direct <- FALSE
    mssg(messages, "\nRetrieving data for taxon '", sci[i], "'\n")
    res <- rcol::cp_nu_search(q=sci[i], status = status,
      minRank = minRank, maxRank = maxRank,
      fuzzy = fuzzy, dataset_key = "3LR", limit = 100)
    df <- res$result
    mm <- NROW(df) > 1

    if (NROW(df) == 0) df <- data.frame(NULL)

    if (NROW(df) == 0) {
      mssg(messages, m_not_found_sp_altclass)
      id <- NA_character_
      att <- "not found"
    } else {
      usage <- df$usage$name
      df <- cbind(id=df$id, usage[,c("scientificName", "rank")],
        status=df$usage$status)
      id <- df$id
      att <- "found"
    }

    # not found
    if (length(id) == 0) {
      mssg(messages, m_not_found_sp_altclass)
      id <- NA_character_
      att <- "not found"
    }

    if (length(id) > 1) {
      # check for exact match
      matchtmp <- df[as.character(df$scientificName) %in% sci[i], "id"]
      if (length(matchtmp) == 1) {
        id <- as.character(matchtmp)
        direct <- TRUE
      } else {
        df <- sub_rows(df, rows)
        if (NROW(df) == 0) {
          id <- NA_character_
          att <- "not found"
        } else {
          id <- df$id
          if (length(id) == 1) {
            rank_taken <- as.character(df$rank)
            att <- "found"
          }
        }

        # more than one found -> user input
        if (length(id) > 1) {
          if (ask) {
            # prompt
            message("\n\n")
            message("\nMore than one COL ID found for taxon '", sci[i], "'!\n
            Enter rownumber of taxon (other inputs will return 'NA'):\n")
            rownames(df) <- 1:NROW(df)
            print(df)
            take <- scan(n = 1, quiet = TRUE, what = 'raw')

            if (length(take) == 0) {
              take <- 'notake'
              att <- 'nothing chosen'
            }
            if (take %in% seq_len(NROW(df))) {
              take <- as.numeric(take)
              message("Input accepted, took id '",
                      as.character(df$id[take]), "'.\n")
              id <- as.character(df$id[take])
              att <- "found"
            } else {
              id <- NA_character_
              att <- "not found"
              mssg(messages, "\nReturned 'NA'!\n\n")
            }
          } else {
            if (length(id) != 1) {
              warning(sprintf(m_more_than_one_found, "col", sci[i]),
                call. = FALSE)
              id <- NA_character_
              att <- m_na_ask_false
            }
          }
        }
      }
    }
    res <- list(id = id, att = att, multiple = mm, direct = direct)
    prog$completed(sci[i], att)
    prog$prog(att)
    tstate$add(sci[i], res)
  }
  out <- tstate$get()
  ids <- structure(as.character(unlist(pluck(out, "id"))), class = "col",
                   match = pluck_un(out, "att", ""),
                   multiple_matches = pluck_un(out, "multiple", logical(1)),
                   pattern_match = pluck_un(out, "direct", logical(1)))
  on.exit(prog$prog_summary(), add = TRUE)
  on.exit(tstate$exit, add = TRUE)
  add_uri(ids, get_url_templates$col)
}

#' @export
#' @rdname get_col
as.col <- function(x, check=FALSE) UseMethod("as.col")

#' @export
#' @rdname get_col
as.col.col <- function(x, check=FALSE) x

#' @export
#' @rdname get_col
as.col.character <- function(x, check=TRUE) if(length(x) == 1) make_col(x, check) else collapse(x, make_col, "col", check=check)

#' @export
#' @rdname get_col
as.col.list <- function(x, check=TRUE) if(length(x) == 1) make_col(x, check) else collapse(x, make_col, "col", check=check)

#' @export
#' @rdname get_col
as.col.data.frame <- function(x, check = TRUE) {
  structure(x$ids, class = "col", match = x$match,
            multiple_matches = x$multiple_matches,
            pattern_match = x$pattern_match, uri = x$uri)
}

#' @export
#' @rdname get_col
as.data.frame.col <- function(x, ...){
  data.frame(ids = as.character(unclass(x)),
             class = "col",
             match = attr(x, "match"),
             multiple_matches = attr(x, "multiple_matches"),
             pattern_match = attr(x, "pattern_match"),
             uri = attr(x, "uri"),
             stringsAsFactors = FALSE)
}

make_col <- function(x, check=TRUE) make_generic(x, get_url_templates$col, "col", check)

check_col <- function(x){
  tryid <- tryCatch(rcol::cp_ds("{key}/taxon/{id}", key = "3LR", id = x),
    error = function(e) e)
  if ( "error" %in% class(tryid) && is.null(tryid$id) ) FALSE else TRUE
}

#' @export
#' @rdname get_col
get_col_ <- function(sci, messages = TRUE, limit = 1000) {
  stats::setNames(lapply(sci, get_col_help, messages = messages,
    limit = limit), sci)
}

get_col_help <- function(sci, messages, limit) {
  mssg(messages, "\nRetrieving data for taxon '", sci, "'\n")
  df <- rcol::cp_nu_search(q=sci, dataset_key = "3LR", limit = limit)
  if (!is.null(df)) df <- nmslwr(df)
  return(df)
}
