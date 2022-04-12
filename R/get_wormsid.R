#' Get Worms ID for a taxon name
#'
#' Retrieve Worms ID of a taxon from World Register of Marine
#' Species (WORMS).
#'
#' @export
#' @param sci_com character; A vector of common or scientific names. Or, a
#' `taxon_state` object (see [taxon-state])
#' @param searchtype character; One of 'scientific' or 'common', or any unique
#' abbreviation
#' @param marine_only logical; marine only? default: `TRUE` (only used
#' when `searchtype="scientific"`); passed on to [worrms::wm_records_name()]
#' @param fuzzy logical; fuzzy search. default: `NULL` (`TRUE` for
#' `searchtype="scientific"` and `FALSE` for `searchtype="common"` to match
#' the default values for those parameters in \pkg{worrms} package); passed on
#' to [worrms::wm_records_name()] or [worrms::wm_records_common()]
#' @param accepted logical; If TRUE, removes names that are not accepted valid
#' names by WORMS. Set to `FALSE` (default) to give back both accepted
#' and unaccepted names.
#' @param ask logical; should get_worms be run in interactive mode?
#' If `TRUE` and more than one worms is found for the species, the
#' user is asked for input. If `FALSE` NA is returned for
#' multiple matches.
#' @param messages logical; should progress be printed?
#' @param rows numeric; Any number from 1 to infinity. If the default NaN, all
#' rows are considered. Note that this function still only gives back a worms
#' class object with one to many identifiers. See [get_worms_()] to get back
#' all, or a subset, of the raw data that you are presented during the ask
#' process.
#' @param query Deprecated, see `sci_com`
#' @param x Input to `as_worms`
#' @param ... Ignored
#' @param check logical; Check if ID matches any existing on the DB, only
#' used in [as_worms()]
#' @template getreturn
#'
#' @family taxonomic-ids
#' @seealso [classification()]
#'
#' @examples \dontrun{
#' (x <- get_worms('Gadus morhua'))
#' data.frame(x)
#'
#' get_worms('Pomatomus saltatrix')
#' get_worms(c("Gadus morhua", "Lichenopora neapolitana"))
#'
#' # marine_only
#' get_worms("Apedinella", marine_only=TRUE)
#' get_worms("Apedinella", marine_only=FALSE)
#'
#' # fuzzy
#' ## searchtype="scientific": fuzzy is TRUE by default
#' get_worms("Platypro", searchtype="scientific", fuzzy=TRUE)
#' get_worms("Platypro", searchtype="scientific", fuzzy=FALSE)
#' ## searchtype="common": fuzzy is FALSE by default
#' get_worms("clam", searchtype="common", fuzzy=FALSE)
#' get_worms("clam", searchtype="common", fuzzy=TRUE)
#'
#' # by common name
#' get_worms("dolphin", 'common')
#' get_worms("clam", 'common')
#'
#' # specify rows to limit choices available
#' get_worms('Plat')
#' get_worms('Plat', rows=1)
#' get_worms('Plat', rows=1:2)
#'
#' # When not found
#' get_worms("howdy")
#' get_worms(c('Gadus morhua', "howdy"))
#'
#' # Convert a worms without class information to a worms class
#' # already a worms, returns the same
#' as_worms(get_worms('Gadus morhua'))
#' # same
#' as_worms(get_worms(c('Gadus morhua', 'Pomatomus saltatrix')))
#' # numeric
#' as_worms(126436)
#' # numeric vector, length > 1
#' as_worms(c(126436,151482))
#' # character
#' as_worms("126436")
#' # character vector, length > 1
#' as_worms(c("126436","151482"))
#' # list, either numeric or character
#' as_worms(list("126436","151482"))
#' ## dont check, much faster
#' as_worms("126436", check=FALSE)
#' as_worms(126436, check=FALSE)
#' as_worms(c("126436","151482"), check=FALSE)
#' as_worms(list("126436","151482"), check=FALSE)
#'
#' (out <- as_worms(c(126436,151482)))
#' data.frame(out)
#' as_worms( data.frame(out) )
#'
#' # Get all data back
#' get_worms_("Plat")
#' get_worms_("Plat", rows=1)
#' get_worms_("Plat", rows=1:2)
#' get_worms_("Plat", rows=1:75)
#' # get_worms_(c("asdfadfasd","Plat"), rows=1:5)
#' }
get_worms <- function(sci_com, searchtype = "scientific", marine_only = TRUE,
  fuzzy = NULL, accepted = FALSE, ask = TRUE, messages = TRUE,
  rows = NA, query = NULL, ...) {

  assert(sci_com, c("character", "taxon_state"))
  assert(searchtype, "character")
  assert(marine_only, "logical")
  assert(fuzzy, "logical")
  assert(accepted, "logical")
  assert(ask, "logical")
  assert(messages, "logical")
  assert_rows(rows)
  pchk(query, "sci_com")

  if (inherits(sci_com, "character")) {
    tstate <- taxon_state$new(class = "worms", names = sci_com)
    items <- sci_com
  } else {
    assert_state(sci_com, "worms")
    tstate <- sci_com
    sci_com <- tstate$taxa_remaining()
    items <- c(sci_com, tstate$taxa_completed())
  }

  prog <- progressor$new(items = items, suppress = !messages)
  done <- tstate$get()
  for (i in seq_along(done)) prog$completed(names(done)[i], done[[i]]$att)
  prog$prog_start()

  for (i in seq_along(sci_com)) {
    direct <- FALSE
    rank_taken <- NA_character_
    name <- NA_character_
    mssg(messages, "\nRetrieving data for taxon '", sci_com[i], "'\n")

    if (!searchtype %in% c("scientific", "common")) {
      stop("'searchtype' must be one of 'scientific' or 'common'", call. = FALSE)
    }
    # NOTE: space replacement needed for curl problem (issue #888)
    query <- gsub(sci_com[i], pattern = ' ', replacement = '+', fixed = TRUE)
    wmdf <- switch(
      searchtype,
      scientific = worms_worker(query, worrms::wm_records_name, rows,
        marine_only, fuzzy %||% TRUE, ...),
      common = worms_worker(query, worrms::wm_records_common, rows,
        marine_only, fuzzy %||% FALSE, ...)
    )
    mm <- NROW(wmdf) > 1

    if (!inherits(wmdf, "tbl_df") || NROW(wmdf) == 0) {
      wmid <- NA_character_
      att <- "not found"
    } else {
      wmdf <- suppressWarnings(data.frame(wmdf))
      wmdf <- wmdf[, c("AphiaID", "scientificname", "authority", "status", "rank")]
      wmdf$rank <- tolower(wmdf$rank)
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
        name <- wmdf$scientificname
        rank_taken <- wmdf$rank
      }

      # check for direct match
      if (nrow(wmdf) > 1) {
        names(wmdf)[grep("scientificname", names(wmdf))] <- "target"
        matchtmp <- wmdf[tolower(wmdf$target) %in% tolower(sci_com[i]), ]
        if (NROW(matchtmp) == 1) {
          wmid <- matchtmp$id
          name <- matchtmp$target
          rank_taken <- matchtmp$rank
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
          message("\nMore than one WORMS ID found for taxon '", sci_com[i], "'!\n
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
            name <- wmdf$target[take]
            rank_taken <- wmdf$rank[take]
            att <- "found"
          } else {
            wmid <- NA_character_
            mssg(messages, "\nReturned 'NA'!\n\n")
            att <- "not found"
          }
        } else {
          if (length(wmid) != 1) {
            warning(sprintf(m_more_than_one_found, "Worms ID", sci_com[i]),
              call. = FALSE)
            wmid <- NA_character_
            att <- m_na_ask_false
          }
        }
      }

    }
    res <- list(id = as.character(wmid), name = name, rank = rank_taken,
      att = att, multiple = mm, direct = direct)
    prog$completed(sci_com[i], att)
    prog$prog(att)
    tstate$add(sci_com[i], res)
  }
  out <- tstate$get()
  res <- make_taxa_taxon(out, "worms")
  on.exit(prog$prog_summary(), add = TRUE)
  on.exit(tstate$exit, add = TRUE)
  return(res)
}
#' @export
#' @rdname get_worms
get_wormsid <- function(...) {
  fchk("get_wormsid", "get_worms")
  get_worms(...)
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
#' @rdname get_worms
as_worms <- function(x, check=TRUE) UseMethod("as_worms")

#' @export
#' @rdname get_worms
as_worms.worms <- function(x, check=TRUE) x

#' @export
#' @rdname get_worms
as_worms.character <- function(x, check=TRUE) if (length(x) == 1) make_worms(x, check) else collapse(x, make_worms, "worms", check = check)

#' @export
#' @rdname get_worms
as_worms.list <- function(x, check=TRUE) if (length(x) == 1) make_worms(x, check) else collapse(x, make_worms, "worms", check = check)

#' @export
#' @rdname get_worms
as_worms.numeric <- function(x, check=TRUE) as_worms(as.character(x), check)

#' @export
#' @rdname get_worms
as_worms.data.frame <- function(x, check=TRUE) as_txid_df(x, check)

make_worms <- function(x, check=TRUE) {
  make_generic(x, 'https://www.marinespecies.org/aphia.php?p=taxdetails&id=%s',
    "worms", check)
}

check_worms <- function(x){
  tt <- worrms::wm_record(as.numeric(x))
  identical(as.character(tt$AphiaID), as.character(x))
}

#' @export
#' @rdname get_worms
get_worms_ <- function(sci_com, messages = TRUE, searchtype = "scientific",
  marine_only = TRUE, fuzzy = NULL, accepted = TRUE, rows = NA, query = NULL,
  ...) {

  pchk(query, "sci_com")
  stats::setNames(
    lapply(sci_com, get_worms_help, messages = messages,
           searchtype = searchtype, marine_only = marine_only, fuzzy = fuzzy,
           accepted = accepted, rows = rows, ...),
    sci_com
  )
}
#' @export
#' @rdname get_worms
get_wormsid_ <- function(...) {
  fchk("get_wormsid_", "get_worms_")
  get_worms_(...)
}

get_worms_help <- function(query, messages, searchtype, marine_only,
  fuzzy, accepted, rows, ...) {

  mssg(messages, "\nRetrieving data for taxon '", query, "'\n")
  searchtype <- match.arg(searchtype, c("scientific", "common"))
  df <- switch(
    searchtype,
    scientific = worms_worker(query, worrms::wm_records_name, rows = rows,
      marine_only = marine_only, fuzzy = fuzzy, ...),
    common = worms_worker(query, worrms::wm_records_common, rows = rows,
      marine_only = marine_only, fuzzy = fuzzy, ...)
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
