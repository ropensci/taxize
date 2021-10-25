#' Get NatureServe taxonomic ID for a taxon name
#'
#' @export
#' @param sci_com character; A vector of common or scientific names. Or, a
#' `taxon_state` object (see [taxon-state])
#' @param searchtype character; One of 'scientific' (default) or 'common'.
#' This doesn't affect the query to NatureServe - but rather affects what
#' column of data is targeted in name filtering post data request.
#' @param ask logical; should get_natserv be run in interactive mode?
#' If `TRUE` and more than one wormsid is found for the species, the
#' user is asked for input. If `FALSE` NA is returned for
#' multiple matches. default: `TRUE`
#' @param messages logical; should progress be printed? default: `TRUE`
#' @param rows numeric; Any number from 1 to infinity. If the default NaN, all
#' rows are considered. Note that this function still only gives back a
#' natserv class object with one to many identifiers. See
#' `get_natserv_()` to get back all, or a subset, of the raw
#' data that you are presented during the ask process.
#' @param x Input to `as_natserv`
#' @param query Deprecated, see `sci_com`
#' @param ... curl options passed on to [crul::verb-POST]
#' @param check logical; Check if ID matches any existing on the DB, only
#' used in [as_natserv()]
#' @template getreturn
#' @family taxonomic-ids
#' @seealso [classification()]
#' @note Authentication no longer required
#' @examples \dontrun{
#' (x <- get_natserv("Helianthus annuus"))
#' library(taxa)
#' tax_id(x)
#'
#' get_natserv('Gadus morhua')
#' get_natserv(c("Helianthus annuus", 'Gadus morhua'))
#'
#' # specify rows to limit choices available
#' get_natserv('Ruby Quaker Moth', 'common')
#' get_natserv('Ruby*', 'common')
#' get_natserv('Ruby*', 'common', rows=1)
#' get_natserv('Ruby*', 'common', rows=1:2)
#'
#' # When not found
#' get_natserv("howdy")
#' get_natserv(c('Gadus morhua', "howdy"))
#'
#' # Convert a natserv without class information to a natserv class
#' # already a natserv, returns the same
#' as_natserv(get_natserv('Pomatomus saltatrix'))
#' # same
#' as_natserv(get_natserv(c('Gadus morhua', 'Pomatomus saltatrix')))
#' # character
#' as_natserv(101905)
#' # character vector, length > 1
#' as_natserv(c(101905, 101998))
#' # list, either numeric or character
#' as_natserv(list(101905, 101998))
#' ## dont check, much faster
#' as_natserv(101905, check = FALSE)
#' as_natserv(c(101905, 101998), check = FALSE)
#' as_natserv(list(101905, 101998), check = FALSE)
#'
#' (out <- as_natserv(c(101905, 101998), check = FALSE))
#' data.frame(out)
#' as_natserv( data.frame(out) )
#'
#' # Get all data back
#' get_natserv_("Helianthus")
#' get_natserv_("Ruby*", searchtype = "common")
#' get_natserv_("Ruby*", searchtype = "common", rows=1:3)
#' }
get_natserv <- function(sci_com, searchtype = "scientific", ask = TRUE,
                          messages = TRUE, rows = NA, query = NULL, ...) {

  assert(sci_com, c("character", "taxon_state"))
  assert(ask, "logical")
  assert(searchtype, "character")
  assert(ask, "logical")
  assert(messages, "logical")
  assert_rows(rows)
  pchk(query, "sci_com")

  if (inherits(sci_com, "character")) {
    tstate <- taxon_state$new(class = "natserv", names = sci_com)
    items <- sci_com
  } else {
    assert_state(sci_com, "natserv")
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
    name <- NA_character_
    nsid <- NA_character_
    mssg(messages, "\nRetrieving data for taxon '", sci_com[i], "'\n")

    if (!searchtype %in% c("scientific", "common")) {
      stop("'searchtype' must be one of 'scientific' or 'common'",
        call. = FALSE)
    }

    nsdf <- ns_worker(x = sci_com[i], searchtype = searchtype, ...)
    mm <- NROW(nsdf) > 1

    if (!inherits(nsdf, "tbl_df") || NROW(nsdf) == 0) {
      att <- "not found"
    } else {
      nsdf <- suppressWarnings(data.frame(nsdf))
      nsdf <- sub_rows(nsdf, rows)

      # should return NA if spec not found
      if (nrow(nsdf) == 0) {
        mssg(messages, m_not_found_sp_altclass)
        att <- 'not found'
      }

      # take the one nsid from data.frame
      if (nrow(nsdf) == 1) {
        nsid <- nsdf$id
        name <- nsdf[[grep(searchtype, names(nsdf), value = TRUE)]]
        att <- 'found'
      }

      # check for direct match
      if (nrow(nsdf) > 1) {

        names(nsdf)[grep(searchtype, names(nsdf))] <- "target"
        # direct <- match(tolower(nsdf$target), tolower(sci_com[i]))
        matchtmp <- nsdf[as.character(nsdf$target) %in% sci_com[i], ]

        if (NROW(matchtmp) == 1) {
          if (!all(is.na(matchtmp))) {
            nsid <- matchtmp$id
            name <- matchtmp$target
            direct <- TRUE
            att <- 'found'
          } else {
            att <- 'not found'
          }
        } else {
          att <- m_na_ask_false_no_direct
          warning("> 1 result; no direct match found", call. = FALSE)
        }
      }

      # multiple matches
      if (any(
        nrow(nsdf) > 1 && is.na(nsid) ||
        nrow(nsdf) > 1 && att == "found" && length(nsid) > 1
      )) {
        if (ask) {
          names(nsdf)[grep(searchtype, names(nsdf))] <- "target"

          # user prompt
          nsdf <- nsdf[order(nsdf$target), ]

          # prompt
          message("\n\n")
          rownames(nsdf) <- seq_len(NROW(nsdf))
          print(nsdf)
          message("\nMore than one NatureServe ID found for taxon '", sci_com[i], "'!\n
                  Enter rownumber of taxon (other inputs will return 'NA'):\n") # prompt
          take <- scan(n = 1, quiet = TRUE, what = 'raw')

          if (length(take) == 0) {
            take <- 'notake'
            att <- 'nothing chosen'
          }
          if (take %in% seq_len(nrow(nsdf))) {
            take <- as.numeric(take)
            message("Input accepted, took taxon '", as.character(nsdf$target[take]), "'.\n")
            nsid <-  nsdf$id[take]
            name <-  nsdf$target[take]
            att <- 'found'
          } else {
            mssg(messages, "\nReturned 'NA'!\n\n")
            att <- 'not found'
          }
        } else {
          if (length(nsid) != 1) {
            warning(sprintf(m_more_than_one_found, "NatureServe ID", sci_com[i]),
              call. = FALSE)
            att <- m_na_ask_false
          }
        }
      }

    }
    res <- list(id = as.character(nsid), name = name, att = att,
      multiple = mm, direct = direct)
    prog$completed(sci_com[i], att)
    prog$prog(att)
    tstate$add(sci_com[i], res)
  }
  out <- tstate$get()
  res <- make_taxa_taxon(out, "natserv", rank = "species")
  on.exit(prog$prog_summary(), add = TRUE)
  on.exit(tstate$exit, add = TRUE)
  return(res)
}
#' @export
#' @rdname get_natserv
get_natservid <- function(...) {
  fchk("get_natservid", "get_natserv")
  get_natserv(...)
}

#' @export
#' @rdname get_natserv
as_natserv <- function(x, check=TRUE) UseMethod("as_natserv")

#' @export
#' @rdname get_natserv
as_natserv.natserv <- function(x, check=TRUE) x

#' @export
#' @rdname get_natserv
as_natserv.character <- function(x, check=TRUE) if (length(x) == 1) make_natserv(x, check) else collapse(x, make_natserv, "natserv", check = check)

#' @export
#' @rdname get_natserv
as_natserv.list <- function(x, check=TRUE) if (length(x) == 1) make_natserv(x, check) else collapse(x, make_natserv, "natserv", check = check)

#' @export
#' @rdname get_natserv
as_natserv.numeric <- function(x, check=TRUE) as_natserv(as.character(x), check)

#' @export
#' @rdname get_natserv
as_natserv.data.frame <- function(x, check=TRUE) as_txid_df(x, check)

make_natserv <- function(x, check=TRUE) {
  make_generic(as.character(x), ns_base_uri(), "natserv", check)
}

check_natserv <- function(x){
  tt <- crul::HttpClient$new(sprintf(ns_base_uri(), x),
    headers = tx_ual)$get()$parse("UTF-8")
  !grepl("No records matched", tt)
}

#' @export
#' @rdname get_natserv
get_natserv_ <- function(sci_com, searchtype = "scientific", messages = TRUE,
  rows = NA, query = NULL, ...) {

  pchk(query, "sci_com")
  stats::setNames(
    lapply(sci_com, get_natserv_help, searchtype = searchtype,
      messages = messages, rows = rows, ...),
    sci_com
  )
}
#' @export
#' @rdname get_natserv
get_natservid_ <- function(...) {
  fchk("get_natservid_", "get_natserv_")
  get_natserv_(...)
}

get_natserv_help <- function(sci_com, searchtype, messages, rows, ...) {
  mssg(messages, "\nRetrieving data for taxon '", sci_com, "'\n")
  df <- ns_worker(x = sci_com, searchtype = searchtype, ...)
  sub_rows(df, rows)
}

ns_base_uri <- function() "https://explorer.natureserve.org/Taxon/ELEMENT_GLOBAL.2.%s"

# x = "Helianthus annuus"; searchtype = "scientific"
# x = "Ruby*"; searchtype = "common"
ns_worker <- function(x, searchtype, ...) {
  query <- switch(searchtype,
    scientific = list(searchToken=x, matchAgainst="allScientificNames", operator="similarTo"),
    common = list(searchToken=x, matchAgainst="allCommonNames", operator="similarTo")
  )
  # FIXME: gotta have pagination, but results not returning as expected, look into natserv bug
  tmp <- tryCatch(
    # natserv::ns_search_spp(text_adv = query, page = 1, per_page = 2000L, ...),
    natserv::ns_search_spp(text_adv = query, ...),
    error = function(e) e)
  if (inherits(tmp, "error")) return(tibble::tibble())
  if (NROW(tmp$results) == 0) return(tibble::tibble())
  tmp <- tmp$results[, c("elementGlobalId", "scientificName", "primaryCommonName", "nsxUrl")]
  names(tmp) <- c('id', 'scientificname', 'commonname', 'uri')
  tmp
}
