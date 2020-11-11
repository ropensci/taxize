#' Get Australian Plant Names Index identifier for a taxon
#'
#' @export
#' @param sci character; A vector of scientific names. Or, a
#' `taxon_state` object (see [taxon-state])
#' @param ask logical; should `get_apni` be run in interactive mode?
#' If `TRUE` and more than one pow is found for teh species, the user is 
#' asked for input. If `FALSE` NA is returned for multiple matches.
#' @param messages logical; should progress be printed?
#' @param ... Curl options passed on to [crul::verb-GET]
#' @param rows numeric; Any number from 1 to infinity. If the default NA, 
#' all rows are considered. Note that this function still only gives back 
#' a pow class object with one to many identifiers. See 
#' [get_apni_()] to get back all, or a subset,
#' of the raw data that you are presented during the ask process.
#' @param rank_filter (character) A taxonomic rank name to filter data after
#' retrieved from APNI. See [rank_ref()] for possible options.
#' Though note that some data sources use atypical ranks, so inspect the data
#' itself for options. Optional. See `Filtering` below.
#' @param x Input to [as.apni()]
#' @param check logical; Check if ID matches any existing on the DB, only 
#' used in [as.apni()]
#' @template getreturn
#' 
#' @family apni
#' 
#' @section Filtering:
#' The parameter `rank_filter` is not used in the search to the data
#' provider, but is used in filtering the data down to a subset that is
#' closer to the target you want.  For these two parameters,
#' you can use regex strings since we use [grep()] internally to match.
#' Filtering narrows down to the set that matches your query, and removes
#' the rest.
#' 
#' @section Pagination:
#' APNI does not allow pagination. It appears that 100 is the max number
#' of results that can be returned. That is, if there's more than 100
#' results *found*, you can only get 100, and no more. So the only 
#' way around it is to refine/change your query.
#'
#' @family taxonomic-ids
#' @seealso [classification()]
#'
#' @examples \dontrun{
#' get_apni("Pinus")
#' get_apni("Acacia ab")
#' get_apni(c("Acacia","Pinus"))
#'
#' # Get back a subset
#' get_apni("Acacia a", rows = 1)
#' get_apni("Acacia a", rows = 1:10)
#'
#' # When not found
#' get_apni("howdy")
#' get_apni(c("Acacia acicularis", "howdy"))
#'
#' # Narrow down results 
#' # to a rank
#' get_apni("Acacia a", rank_filter = "species", rows = 1:10)
#'
#' # Convert a apni without class information to a apni class
#' z <- get_apni("Acacia abbreviata")
#' # already a apni, returns the same
#' as.apni(z)
#' as.apni(57932)
#' as.apni("57932")
#' # character vector, length > 1
#' ids <- c("57932","57943")
#' as.apni(ids)
#' # list, with character strings
#' as.apni(as.list(ids)) 
#' ## dont check, much faster
#' as.apni("57932", check=FALSE)
#' as.apni(ids, check=FALSE)
#' as.apni(as.list(ids), check=FALSE)
#'
#' (out <- as.apni(ids))
#' data.frame(out)
#' as.apni( data.frame(out) )
#'
#' # Get all data back
#' get_apni_("Quercus")
#' get_apni_("Acacia")
#' get_apni_("Quercus", rows=1:5)
#' get_apni_("Quercus", rows=1)
#' get_apni_(c("Pinus", "Abies"), rows = 1:3)
#' }

get_apni <- function(sci, ask = TRUE, messages = TRUE, rows = NA,
  rank_filter = NULL, ...) {

  assert(sci, c("character", "taxon_state"))
  assert(ask, "logical")
  assert(messages, "logical")
  assert(rank_filter, "character")
  assert_rows(rows)

  if (inherits(sci, "character")) {
    tstate <- taxon_state$new(class = "apni", names = sci)
    items <- sci
  } else {
    assert_state(sci, "apni")
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
    apni_df <- apni_acceptable_names(q = sci[i], ...)[[1]]
    mm <- NROW(apni_df) > 1

    if (NROW(apni_df) == 0) {
      apni <- NA_character_
      att <- "not found"
    } else {
      apni_df <- apni_df[, c("name_simple","rank","status","id")]

      # should return NA if spec not found
      if (nrow(apni_df) == 0) {
        mssg(messages, "Not found. Consider checking the spelling or alternate classification")
        apni <- NA_character_
        att <- 'not found'
      }

      # take the one apni from data.frame
      if (nrow(apni_df) == 1) {
        apni <- apni_df$id
        att <- 'found'
      }
      # check for direct match
      if (nrow(apni_df) > 1) {
        names(apni_df)[grep('name_simple', names(apni_df))] <- "target"
        di_rect <- apni_df[tolower(apni_df$target) %in% tolower(sci[i]), "id"]
        if (NROW(di_rect) == 1) {
          apni <- di_rect
          direct <- TRUE
          att <- 'found'
        } else {
          apni <- NA_character_
          direct <- FALSE
          att <- 'found'
        }
      }
      # multiple matches
      if (any(
        nrow(apni_df) > 1 && is.na(apni) |
        nrow(apni_df) > 1 && att == "found" && length(apni) > 1
      )) {
        if (!is.null(rank_filter)) {
          apni_df <- filt(apni_df, "rank", rank_filter)
        }

        apni_df <- sub_rows(apni_df, rows)
        apni <- id <- apni_df$id
        if (length(id) == 1) {
          direct <- TRUE
          att <- "found"
        }

        if (ask) {
          # user prompt
          apni_df <- apni_df[order(apni_df$target), ]
          apni_df <- tibble::remove_rownames(apni_df)
          # rownames(apni_df) <- 1:nrow(apni_df)
          if (length(apni) > 1 || NROW(apni_df) > 1) {
            # prompt
            message("\n\n")
            print(apni_df)
            message("\nMore than one apni found for taxon '", sci[i], "'!\n
          Enter rownumber of taxon (other inputs will return 'NA'):\n") # prompt
            take <- scan(n = 1, quiet = TRUE, what = 'raw')

            if (length(take) == 0)
              take <- 'notake'
            if (take %in% seq_len(nrow(apni_df))) {
              take <- as.numeric(take)
              message("Input accepted, took taxon '", as.character(apni_df$target[take]), "'.\n")
              apni <-  apni_df$id[take]
              att <- 'found'
            } else {
              apni <- NA_character_
              mssg(messages, "\nReturned 'NA'!\n\n")
              att <- 'not found'
            }
          }
        } else {
          if (length(apni) == 1) {
            att <- "found"
          } else {
            warning(
              sprintf("More than one apni found for taxon '%s'; refine query or set ask=TRUE",
                      sci[i]),
              call. = FALSE
            )
            apni <- NA_character_
            att <- 'NA due to ask=FALSE & > 1 result'
          }
        }
      }
    }
    res <- list(id = as.character(apni), att = att, multiple = mm,
      direct = direct)
    prog$completed(sci[i], att)
    prog$prog(att)
    tstate$add(sci[i], res)
  }
  out <- tstate$get()
  ids <- structure(as.character(unlist(pluck(out, "id"))), class = "apni",
                   match = pluck_un(out, "att", ""),
                   multiple_matches = pluck_un(out, "multiple", logical(1)),
                   pattern_match = pluck_un(out, "direct", logical(1)))
  on.exit(prog$prog_summary(), add = TRUE)
  on.exit(tstate$exit, add = TRUE)
  add_uri(ids, get_url_templates$apni)
}

#' @export
#' @rdname get_apni
as.apni <- function(x, check=TRUE) UseMethod("as.apni")

#' @export
#' @rdname get_apni
as.apni.apni <- function(x, check=TRUE) x

#' @export
#' @rdname get_apni
as.apni.numeric <- function(x, check=TRUE) as.apni(as.character(x))

#' @export
#' @rdname get_apni
as.apni.character <- function(x, check=TRUE) if(length(x) == 1) make_apni(x, check) else collapse(x, make_apni, "apni", check=check)

#' @export
#' @rdname get_apni
as.apni.list <- function(x, check=TRUE) if(length(x) == 1) make_apni(x, check) else collapse(x, make_apni, "apni", check=check)

#' @export
#' @rdname get_apni
as.apni.data.frame <- function(x, check=TRUE) {
  structure(x$ids, class="apni", match=x$match,
            multiple_matches = x$multiple_matches,
            pattern_match = x$pattern_match, uri=x$uri)
}

#' @export
#' @rdname get_apni
as.data.frame.apni <- function(x, ...){
  data.frame(ids = as.character(unclass(x)),
             class = "apni",
             match = attr(x, "match"),
             multiple_matches = attr(x, "multiple_matches"),
             pattern_match = attr(x, "pattern_match"),
             uri = attr(x, "uri"),
             stringsAsFactors = FALSE)
}

make_apni <- function(x, check=TRUE) {
  make_generic(x, get_url_templates$apni, "apni", check)
}

check_apni <- function(x) apni_id_exists(x)

#' @export
#' @rdname get_apni
get_apni_ <- function(x, messages = TRUE, rows = NA, ...){
  stats::setNames(lapply(x, get_apni_help, messages = messages, rows = rows, ...), x)
}

get_apni_help <- function(x, messages, rows, ...){
  mssg(messages, "\nRetrieving data for taxon '", x, "'\n")
  df <- apni_acceptable_names(q = x, ...)
  if (NROW(df) == 0) NULL else sub_rows(df, rows)
}
