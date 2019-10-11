#' Get the TSN code for a search term.
#'
#' Retrieve the taxonomic serial numbers (TSN) of a taxon from ITIS.
#'
#' @export
#' @param searchterm character; A vector of common or scientific names.
#' Or, a `taxon_state` object (see [taxon-state])
#' @param searchtype character; One of 'scientific' or 'common', or any
#' unique abbreviation
#' @param accepted logical; If TRUE, removes names that are not accepted valid
#' names by ITIS. Set to `FALSE` (default) to give back both accepted
#' and unaccepted names.
#' @param ask logical; should get_tsn be run in interactive mode?
#' If `TRUE` and more than one TSN is found for the species, the user is
#' asked for input. If `FALSE` NA is returned for multiple matches.
#' @param messages logical; should progress be printed?
#' @param rows numeric; Any number from 1 to infinity. If the default NA, all
#' rows are considered. Note that this function still only gives back a tsn
#' class object with one to many identifiers. See
#' [get_tsn_()] to get back all, or a subset, of the raw
#' data that you are presented during the ask process.
#' @param x Input to as.tsn
#' @param ... Ignored
#' @param check logical; Check if ID matches any existing on the DB, only
#' used in [as.tsn()]
#' @template getreturn
#'
#' @family taxonomic-ids
#' @seealso [classification()]
#'
#' @examples \dontrun{
#' # the new way
#' x <- get_tsn("Quercus douglasii")
#' x[[1]]
#' x[[1]]$name
#' x[[1]]$attributes
#' x[[1]]$is_empty()
#'
#' splist <- c("annona cherimola", 'annona muricata', "quercus robur",
#'    "shorea robusta", "pandanus patina", "oryza sativa", "durio zibethinus")
#' res <- get_tsn(splist)
#' lapply(res, function(w) w$name)
#' vapply(res, function(w) w$name$name, "")
#' vapply(res, function(w) w$attributes$pattern_match, logical(1))
#'
#'
#' # old way
#' get_tsn("Quercus douglasii")
#' get_tsn("Chironomus riparius")
#' get_tsn(c("Chironomus riparius","Quercus douglasii"))
#' splist <- c("annona cherimola", 'annona muricata', "quercus robur",
#' 		"shorea robusta", "pandanus patina", "oryza sativa", "durio zibethinus")
#' get_tsn(splist, messages=FALSE)
#'
#' # specify rows to limit choices available
#' get_tsn('Arni')
#' get_tsn('Arni', rows=1)
#' get_tsn('Arni', rows=1:2)
#'
#' # When not found
#' get_tsn("howdy")
#' get_tsn(c("Chironomus riparius", "howdy"))
#'
#' # Using common names
#' get_tsn(searchterm="black bear", searchtype="common")
#'
#' # Convert a tsn without class information to a tsn class
#' as.tsn(get_tsn("Quercus douglasii")) # already a tsn, returns the same
#' as.tsn(get_tsn(c("Chironomus riparius","Pinus contorta"))) # same
#' as.tsn(19322) # numeric
#' as.tsn(c(19322,129313,506198)) # numeric vector, length > 1
#' as.tsn("19322") # character
#' as.tsn(c("19322","129313","506198")) # character vector, length > 1
#' as.tsn(list("19322","129313","506198")) # list, either numeric or character
#' ## dont check, much faster
#' as.tsn("19322", check=FALSE)
#' as.tsn(19322, check=FALSE)
#' as.tsn(c("19322","129313","506198"), check=FALSE)
#' as.tsn(list("19322","129313","506198"), check=FALSE)
#'
#' (out <- as.tsn(c(19322,129313,506198)))
#' data.frame(out)
#' as.tsn( data.frame(out) )
#'
#' # Get all data back
#' get_tsn_("Arni")
#' get_tsn_("Arni", rows=1)
#' get_tsn_("Arni", rows=1:2)
#' get_tsn_(c("asdfadfasd","Pinus contorta"), rows=1:5)
#' }

get_tsn <- function(searchterm, searchtype = "scientific", accepted = FALSE,
                    ask = TRUE, messages = TRUE, rows = NA, ...) {

  assert(searchterm, c("character", "taxon_state"))
  assert(ask, "logical")
  assert(messages, "logical")
  assert(searchtype, "character")
  assert(accepted, "logical")
  assert_rows(rows)

  if (inherits(searchterm, "character")) {
    tstate <- taxon_state$new(class = "tsn", names = searchterm)
    items <- searchterm
  } else {
    assert_state(searchterm, "tsn")
    tstate <- searchterm
    searchterm <- tstate$taxa_remaining()
    items <- c(searchterm, tstate$taxa_completed())
  }

  prog <- progressor$new(items = items, suppress = !messages)
  done <- tstate$get()
  for (i in seq_along(done)) prog$completed(names(done)[i], done[[i]]$att)
  prog$prog_start()

  for (i in seq_along(searchterm)) {
    direct <- FALSE
    mssg(messages, "\nRetrieving data for taxon '", searchterm[i], "'\n")
    targname <- NA_character_

    searchtype <- match.arg(searchtype, c("scientific", "common"))
    tsn_df <- ritis::terms(searchterm[i], what = searchtype, ...)
    mm <- NROW(tsn_df) > 1

    if (!inherits(tsn_df, "data.frame") || NROW(tsn_df) == 0) {
      tsn <- NA_character_
      att <- "not found"
    } else {
      if ("commonNames" %in% names(tsn_df)) {
        tsn_df$commonNames <-
          sapply(tsn_df$commonNames, function(z) paste0(z, collapse = ","))
      }

      tsn_df <- tsn_df[, c("tsn", "scientificName", "commonNames", "nameUsage")]

      if (accepted) {
        tsn_df <- tsn_df[ tsn_df$nameUsage %in% c("valid", "accepted"), ]
      }

      tsn_df <- sub_rows(tsn_df, rows)

      # should return NA if spec not found
      if (NROW(tsn_df) == 0) {
        mssg(messages, m_not_found_sp_altclass)
        tsn <- NA_character_
        att <- "not found"
      }

      # take the one tsn from data.frame
      if (NROW(tsn_df) == 1) {
        tsn <- tsn_df$tsn
        att <- 'found'
        targname <- tsn_df$scientificName
      }

      # check for direct match
      if (NROW(tsn_df) > 1) {
        tsn_df <- data.frame(tsn_df, stringsAsFactors = FALSE)
        names(tsn_df)[grep(searchtype, names(tsn_df))] <- "target"
        matchtmp <- tsn_df[tolower(tsn_df$target) %in% tolower(searchterm[i]), "tsn"]
        if (length(matchtmp) == 1) {
          tsn <- matchtmp
          direct <- TRUE
          att <- "found"
        } else {
          direct <- FALSE
          tsn <- NA_character_
          att <- m_na_ask_false_no_direct
          warning("> 1 result; no direct match found", call. = FALSE)
        }
      }

      # multiple matches
      if (any(
        NROW(tsn_df) > 1 && is.na(tsn) |
        NROW(tsn_df) > 1 && att == "found" && length(tsn) > 1
      )) {
        if (ask) {
          names(tsn_df)[grep(searchtype, names(tsn_df))] <- "target"
          # user prompt
          tsn_df <- tsn_df[order(tsn_df$target), ]
          rownames(tsn_df) <- NULL

          # prompt
          message("\n\n")
          print(tsn_df)
          message("\nMore than one TSN found for taxon '", searchterm[i], "'!\n
            Enter rownumber of taxon (other inputs will return 'NA'):\n")
          take <- scan(n = 1, quiet = TRUE, what = "raw")

          if (length(take) == 0) {
            take <- "notake"
            att <- "nothing chosen"
          }
          if (take %in% seq_len(NROW(tsn_df))) {
            take <- as.numeric(take)
            message("Input accepted, took taxon '",
              as.character(tsn_df$target[take]), "'.\n")
            tsn <-  tsn_df$tsn[take]
            att <- 'found'
            targname <- tsn_df$target[take]
          } else {
            tsn <- NA_character_
            mssg(messages, "\nReturned 'NA'!\n\n")
            att <- "not found"
          }
        } else {
          if (length(tsn) != 1) {
            warning(sprintf(m_more_than_one_found, "tsn", searchterm[i]),
              call. = FALSE)
            tsn <- NA_character_
            att <- m_na_ask_false
          }
        }
      }

    }
    res <- list(id = as.character(tsn), name = targname,
      att = att, multiple = mm, direct = direct)
    prog$completed(searchterm[i], att)
    prog$prog(att)
    tstate$add(searchterm[i], res)
  }

  out <- tstate$get()
  res <- taxa::taxa(.list = lapply(out, function(z) {
    if (is.na(z$name)) return(taxa::taxon(NULL))
    url <- sprintf(get_url_templates$itis, z$id)
    rk <- if (!is.na(z$id)) tolower(ritis::rank_name(z$id)$rankname) else ""
    out <- taxa::taxon(
      taxa::taxon_name(z$name %||% "", taxa::database_list$itis),
      taxa::taxon_rank(rk, taxa::database_list$itis),
      taxa::taxon_id(z$id %||% "", url, taxa::database_list$itis)
    )
    out$attributes <- list(
      match = z$att,
      multiple_matches = z$multiple,
      pattern_match = z$direct
    )
    out
  }))
  on.exit(prog$prog_summary(), add = TRUE)
  on.exit(tstate$exit, add = TRUE)
  return(res)
}

#' @export
#' @rdname get_tsn
as.tsn <- function(x, check = TRUE) UseMethod("as.tsn")

#' @export
#' @rdname get_tsn
as.tsn.tsn <- function(x, check = TRUE) x

#' @export
#' @rdname get_tsn
as.tsn.Taxa <- function(x, check = TRUE) x

#' @export
#' @rdname get_tsn
as.tsn.character <- function(x, check = TRUE) if (length(x) == 1) make_tsn(x, check) else collapse(x, make_tsn, "tsn", check = check)

#' @export
#' @rdname get_tsn
as.tsn.list <- function(x, check = TRUE) if (length(x) == 1) make_tsn(x, check) else collapse(x, make_tsn, "tsn", check = check)

#' @export
#' @rdname get_tsn
as.tsn.numeric <- function(x, check = TRUE) as.tsn(as.character(x), check)

#' @export
#' @rdname get_tsn
as.tsn.data.frame <- function(x, check = TRUE) {
  # structure(x$ids, class = "tsn", match = x$match,
  #           multiple_matches = x$multiple_matches,
  #           pattern_match = x$pattern_match, uri = x$uri)
  stop("FIXME: need to change this to convert to Taxa object")
}

#' @export
#' @rdname get_tsn
as.data.frame.tsn <- function(x, ...){
  data.frame(
    ids = vapply(x$ids, "[[", "", "id"),
    class = "tsn",
    match = vapply(x$taxa, function(z) z$attributes$match, ""),
    multiple_matches = vapply(x$taxa, function(z) z$attributes$multiple_matches, logical(1)),
    pattern_match = vapply(x$taxa, function(z) z$attributes$pattern_match, logical(1)),
    uri = vapply(x$ids, "[[", "", "url"),
    stringsAsFactors = FALSE)
}

make_tsn <- function(x, check=TRUE) {
  make_generic(x, get_url_templates$itis, "tsn", check)
}

check_tsn <- function(x){
  tt <- suppressMessages(itis_getrecord(x))
  valid <- identical(tt$acceptedNameList$tsn, as.character(x))
  structure(valid,
    name = tt$scientificName$combinedName,
    rank = tolower(gsub("^\\s+|\\s+$", "", tt$taxRank$rankName)))
}

#' @export
#' @rdname get_tsn
get_tsn_ <- function(searchterm, messages = TRUE, searchtype = "scientific",
                     accepted = TRUE, rows = NA, ...) {
  stats::setNames(
    lapply(searchterm, get_tsn_help, messages = messages,
           searchtype = searchtype, accepted = accepted, rows = rows, ...),
    searchterm
  )
}

get_tsn_help <- function(searchterm, messages, searchtype, accepted,
  rows, ...) {

  mssg(messages, "\nRetrieving data for taxon '", searchterm, "'\n")
  searchtype <- match.arg(searchtype, c("scientific", "common"))
  df <- ritis::terms(searchterm, what = searchtype, ...)
  if (!inherits(df, "tbl_df") || NROW(df) == 0) {
    NULL
  } else {
    df <- df[,c("tsn", "scientificName", "commonNames", "nameUsage")]
    if ("commonNames" %in% names(df)) {
      df$commonNames <-
        sapply(df$commonNames, function(z) paste0(z, collapse = ","))
    }
    if (accepted) df <- df[ df$nameUsage %in% c("valid", "accepted"), ]
    sub_rows(df, rows)
  }
}
