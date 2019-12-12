#' Get Worms ID for a taxon name
#'
#' Retrieve Worms ID of a taxon from World Register of Marine
#' Species (WORMS).
#'
#' @export
#' @param query character; A vector of common or scientific names. Or, a
#' `taxon_state` object (see [taxon-state])
#' @param searchtype character; One of 'scientific' or 'common', or any unique
#' abbreviation
#' @param marine_only logical; marine only? default: ‘TRUE (only used
#' when `searchtype="scientific"`); passed on to [worrms::wm_records_name()]
#' @param fuzzy logical; fuzzy search. default: `NULL` (`TRUE` for
#' `searchtype="scientific"` and `FALSE` for `searchtype="common"` to match
#' the default values for those parameters in \pkg{worrms} package); passed on
#' to [worrms::wm_records_name()] or [worrms::wm_records_common()]
#' @param accepted logical; If TRUE, removes names that are not accepted valid
#' names by WORMS. Set to `FALSE` (default) to give back both accepted
#' and unaccepted names.
#' @param ask logical; should get_wormsid be run in interactive mode?
#' If `TRUE` and more than one wormsid is found for the species, the
#' user is asked for input. If `FALSE` NA is returned for
#' multiple matches.
#' @param messages logical; should progress be printed?
#' @param rows numeric; Any number from 1 to infinity. If the default NaN, all
#' rows are considered. Note that this function still only gives back a wormsid
#' class object with one to many identifiers. See [get_wormsid_()] to get back
#' all, or a subset, of the raw data that you are presented during the ask
#' process.
#' @param x Input to as.wormsid
#' @param ... Ignored
#' @param check logical; Check if ID matches any existing on the DB, only
#' used in [as.wormsid()]
#' @template getreturn
#'
#' @family taxonomic-ids
#' @seealso [classification()]
#'
#' @examples \dontrun{
#' (x <- get_wormsid('Gadus morhua'))
#' attributes(x)
#' attr(x, "match")
#' attr(x, "multiple_matches")
#' attr(x, "pattern_match")
#' attr(x, "uri")
#' 
#' get_wormsid('Pomatomus saltatrix')
#' get_wormsid(c("Gadus morhua", "Lichenopora neapolitana"))
#'
#' # marine_only
#' get_wormsid("Apedinella", marine_only=TRUE)
#' get_wormsid("Apedinella", marine_only=FALSE)
#' 
#' # fuzzy
#' ## searchtype="scientific": fuzzy is TRUE by default
#' get_wormsid("Platypro", searchtype="scientific", fuzzy=TRUE)
#' get_wormsid("Platypro", searchtype="scientific", fuzzy=FALSE)
#' ## searchtype="common": fuzzy is FALSE by default
#' get_wormsid("clam", searchtype="common", fuzzy=FALSE)
#' get_wormsid("clam", searchtype="common", fuzzy=TRUE)
#' 
#' # by common name
#' get_wormsid("dolphin", 'common')
#' get_wormsid("clam", 'common')
#'
#' # specify rows to limit choices available
#' get_wormsid('Plat')
#' get_wormsid('Plat', rows=1)
#' get_wormsid('Plat', rows=1:2)
#'
#' # When not found
#' get_wormsid("howdy")
#' get_wormsid(c('Gadus morhua', "howdy"))
#'
#' # Convert a wormsid without class information to a wormsid class
#' # already a wormsid, returns the same
#' as.wormsid(get_wormsid('Gadus morhua'))
#' # same
#' as.wormsid(get_wormsid(c('Gadus morhua', 'Pomatomus saltatrix')))
#' # numeric
#' as.wormsid(126436)
#' # numeric vector, length > 1
#' as.wormsid(c(126436,151482))
#' # character
#' as.wormsid("126436")
#' # character vector, length > 1
#' as.wormsid(c("126436","151482"))
#' # list, either numeric or character
#' as.wormsid(list("126436","151482"))
#' ## dont check, much faster
#' as.wormsid("126436", check=FALSE)
#' as.wormsid(126436, check=FALSE)
#' as.wormsid(c("126436","151482"), check=FALSE)
#' as.wormsid(list("126436","151482"), check=FALSE)
#'
#' (out <- as.wormsid(c(126436,151482)))
#' data.frame(out)
#' as.wormsid( data.frame(out) )
#'
#' # Get all data back
#' get_wormsid_("Plat")
#' get_wormsid_("Plat", rows=1)
#' get_wormsid_("Plat", rows=1:2)
#' get_wormsid_("Plat", rows=1:75)
#' # get_wormsid_(c("asdfadfasd","Plat"), rows=1:5)
#' }
get_wormsid <- function(query, searchtype = "scientific", marine_only = TRUE,
  fuzzy = NULL, accepted = FALSE, ask = TRUE, messages = TRUE,
  rows = NA, ...) {

  assert(query, c("character", "taxon_state"))
  assert(searchtype, "character")
  assert(marine_only, "logical")
  assert(fuzzy, "logical")
  assert(accepted, "logical")
  assert(ask, "logical")
  assert(messages, "logical")
  assert_rows(rows)

  if (inherits(query, "character")) {
    tstate <- taxon_state$new(class = "wormsid", names = query)
    items <- query
  } else {
    assert_state(query, "wormsid")
    tstate <- query
    query <- tstate$taxa_remaining()
    items <- c(query, tstate$taxa_completed())
  }

  prog <- progressor$new(items = items, suppress = !messages)
  done <- tstate$get()
  for (i in seq_along(done)) prog$completed(names(done)[i], done[[i]]$att)
  prog$prog_start()

  for (i in seq_along(query)) {
    direct <- FALSE
    mssg(messages, "\nRetrieving data for taxon '", query[i], "'\n")

    if (!searchtype %in% c("scientific", "common")) {
      stop("'searchtype' must be one of 'scientific' or 'common'", call. = FALSE)
    }
    wmdf <- switch(
      searchtype,
      scientific = worms_worker(query[i], worrms::wm_records_name, rows,
        marine_only, fuzzy %||% TRUE, ...),
      common = worms_worker(query[i], worrms::wm_records_common, rows,
        marine_only, fuzzy %||% FALSE, ...)
    )
    mm <- NROW(wmdf) > 1

    if (!inherits(wmdf, "tbl_df") || NROW(wmdf) == 0) {
      wmid <- NA_character_
      att <- "not found"
    } else {
      wmdf <- suppressWarnings(data.frame(wmdf))
      wmdf <- wmdf[, c("AphiaID", "scientificname", "authority", "status")]
      names(wmdf)[1] <- "id"

      if (accepted) {
        wmdf <- wmdf[ wmdf$status %in% 'accepted', ]
      }

      wmdf <- sub_rows(wmdf, rows)

      # should return NA if spec not found
      if (nrow(wmdf) == 0) {
        mssg(messages, m_not_found_sp_altclass)
        wmid <- NA_character_
        att <- "not found"
      }

      # take the one wmid from data.frame
      if (nrow(wmdf) == 1) {
        wmid <- wmdf$id
        att <- "found"
      }

      # check for direct match
      if (nrow(wmdf) > 1) {
        names(wmdf)[grep("scientificname", names(wmdf))] <- "target"
        matchtmp <- wmdf[tolower(wmdf$target) %in% tolower(query[i]), "id"]
        if (length(matchtmp) == 1) {
          wmid <- matchtmp
          direct <- TRUE
          att <- "found"
        } else {
          wmid <- NA_character_
          att <- "not found"
        }
      }

      # multiple matches
      if (any(
        nrow(wmdf) > 1 && is.na(wmid) |
        nrow(wmdf) > 1 && att == "found" & length(wmid) > 1
      )) {
        if (ask) {
          names(wmdf)[grep("scientificname", names(wmdf))] <- "target"

          # user prompt
          wmdf <- wmdf[order(wmdf$target), ]

          # prompt
          message("\n\n")
          print(wmdf)
          message("\nMore than one WORMS ID found for taxon '", query[i], "'!\n
                  Enter rownumber of taxon (other inputs will return 'NA'):\n") # prompt
          take <- scan(n = 1, quiet = TRUE, what = 'raw')

          if (length(take) == 0) {
            take <- 'notake'
            att <- 'nothing chosen'
          }
          if (take %in% seq_len(nrow(wmdf))) {
            take <- as.numeric(take)
            message("Input accepted, took taxon '", as.character(wmdf$target[take]), "'.\n")
            wmid <-  wmdf$id[take]
            att <- "found"
          } else {
            wmid <- NA_character_
            mssg(messages, "\nReturned 'NA'!\n\n")
            att <- "not found"
          }
        } else {
          if (length(wmid) != 1) {
            warning(sprintf(m_more_than_one_found, "Worms ID", query[i]), 
              call. = FALSE)
            wmid <- NA_character_
            att <- m_na_ask_false
          }
        }
      }

    }
    res <- list(id = as.character(wmid), att = att, multiple = mm,
      direct = direct)
    prog$completed(query[i], att)
    prog$prog(att)
    tstate$add(query[i], res)
  }
  out <- tstate$get()
  ids <- structure(pluck_un(out, "id", ""), class = "wormsid",
    match = pluck_un(out, "att", ""),
    multiple_matches = pluck_un(out, "multiple", logical(1)),
    pattern_match = pluck_un(out, "direct", logical(1))
  )
  on.exit(prog$prog_summary(), add = TRUE)
  on.exit(tstate$exit, add = TRUE)
  add_uri(ids, get_url_templates$worms)
}

try_df <- function(expr) {
  res <- tryCatch(expr, error = function(e) e)
  if (inherits(res, "error")) {
    data.frame(NULL)
  } else {
    res
  }
}

#' @export
#' @rdname get_wormsid
as.wormsid <- function(x, check=TRUE) UseMethod("as.wormsid")

#' @export
#' @rdname get_wormsid
as.wormsid.wormsid <- function(x, check=TRUE) x

#' @export
#' @rdname get_wormsid
as.wormsid.character <- function(x, check=TRUE) if (length(x) == 1) make_worms(x, check) else collapse(x, make_worms, "wormsid", check = check)

#' @export
#' @rdname get_wormsid
as.wormsid.list <- function(x, check=TRUE) if (length(x) == 1) make_worms(x, check) else collapse(x, make_worms, "wormsid", check = check)

#' @export
#' @rdname get_wormsid
as.wormsid.numeric <- function(x, check=TRUE) as.wormsid(as.character(x), check)

#' @export
#' @rdname get_wormsid
as.wormsid.data.frame <- function(x, check=TRUE) {
  structure(x$ids, class = "wormsid", match = x$match,
            multiple_matches = x$multiple_matches,
            pattern_match = x$pattern_match, uri = x$uri)
}

#' @export
#' @rdname get_wormsid
as.data.frame.wormsid <- function(x, ...){
  data.frame(ids = as.character(unclass(x)),
             class = "wormsid",
             match = attr(x, "match"),
             multiple_matches = attr(x, "multiple_matches"),
             pattern_match = attr(x, "pattern_match"),
             uri = attr(x, "uri"),
             stringsAsFactors = FALSE)
}

make_worms <- function(x, check=TRUE) make_generic(x, 'http://www.marinespecies.org/aphia.php?p=taxdetails&id=%s', "wormsid", check)

check_wormsid <- function(x){
  tt <- worrms::wm_record(as.numeric(x))
  identical(as.character(tt$AphiaID), as.character(x))
}

#' @export
#' @rdname get_wormsid
get_wormsid_ <- function(query, messages = TRUE, searchtype = "scientific",
                       accepted = TRUE, rows = NA, ...) {
  stats::setNames(
    lapply(query, get_wormsid_help, messages = messages,
           searchtype = searchtype, accepted = accepted, rows = rows, ...),
    query
  )
}

get_wormsid_help <- function(query, messages, searchtype, accepted, rows, ...) {
  mssg(messages, "\nRetrieving data for taxon '", query, "'\n")
  searchtype <- match.arg(searchtype, c("scientific", "common"))
  df <- switch(
    searchtype,
    scientific = worms_worker(query, worrms::wm_records_name, rows, ...),
    common = worms_worker(query, worrms::wm_records_common, rows, ...)
  )
  if (!inherits(df, "tbl_df") || NROW(df) == 0) {
    NULL
  } else {
    df <- df[, c("AphiaID","scientificname","authority","status")]
    if (accepted) df <- df[ df$status %in% 'accepted', ]
    sub_rows(df, rows)
  }
}

# WORMS WORKER
# worms_worker(x = "Plat", expr = worrms::wm_records_name)
worms_worker <- function(x, expr, rows, marine_only, fuzzy, ...) {
  if (
    all(!is.na(rows)) &&
    class(rows) %in% c('numeric', 'integer') &&
    rows[length(rows)] <= 50
  ) {
    expr(x, marine_only = marine_only, fuzzy = fuzzy, ...)
  } else if (
    all(!is.na(rows)) &&
    class(rows) %in% c('numeric', 'integer') &&
    rows[length(rows)] > 50
  ) {
    out <- try_df(expr(x, marine_only = marine_only, fuzzy = fuzzy, ...))
    out <- list(out)
    i <- 1
    total <- 0
    while (NROW(out[[length(out)]]) == 50 && total < rows[length(rows)]) {
      i <- i + 1
      out[[i]] <- try_df(expr(x, marine_only = marine_only, fuzzy = fuzzy,
        offset = sum(unlist(sapply(out, NROW))), ...))
      total <- sum(unlist(sapply(out, NROW)))
    }
    df2dt2tbl(out)[rows,]
  } else {
    out <- try_df(expr(x, marine_only = marine_only, fuzzy = fuzzy, ...))
    out <- list(out)
    i <- 1
    while (NROW(out[[length(out)]]) == 50) {
      i <- i + 1
      out[[i]] <- try_df(expr(x, marine_only = marine_only, fuzzy = fuzzy,
        offset = sum(unlist(sapply(out, NROW))), ...))
    }
    df2dt2tbl(out)
  }
}
