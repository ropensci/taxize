#' Get the TSN code for a search term.
#'
#' Retrieve the taxonomic serial numbers (TSN) of a taxon from ITIS.
#'
#' @export
#' @param sci_com character; A vector of common or scientific names.
#' Or, a `taxon_state` object (see [taxon-state])
#' @param searchtype character; One of 'scientific' or 'common', or any
#' unique abbreviation
#' @param accepted logical; If TRUE, removes names that are not accepted valid
#' names by ITIS. Set to `FALSE` (default) to give back both accepted
#' and unaccepted names.
#' @param ask logical; should get_itis be run in interactive mode?
#' If `TRUE` and more than one TSN is found for the species, the user is
#' asked for input. If `FALSE` NA is returned for multiple matches.
#' @param messages logical; should progress be printed?
#' @param rows numeric; Any number from 1 to infinity. If the default NA, all
#' rows are considered. Note that this function still only gives back a tsn
#' class object with one to many identifiers. See
#' [get_itis_()] to get back all, or a subset, of the raw
#' data that you are presented during the ask process.
#' @param x Input to as.itis
#' @param searchterm Deprecated, see `sci_com`
#' @param ... Ignored
#' @param check logical; Check if ID matches any existing on the DB, only
#' used in [as.itis()]
#' @template getreturn
#'
#' @family taxonomic-ids
#' @seealso [classification()]
#'
#' @examples \dontrun{
#' get_itis("Quercus douglasii")
#' get_itis("Chironomus riparius")
#' get_itis(c("Chironomus riparius","Quercus douglasii"))
#' splist <- c("annona cherimola", 'annona muricata', "quercus robur",
#' 		"shorea robusta", "pandanus patina", "oryza sativa", "durio zibethinus")
#' get_itis(splist, messages=FALSE)
#'
#' # specify rows to limit choices available
#' get_itis('Arni')
#' get_itis('Arni', rows=1)
#' get_itis('Arni', rows=1:2)
#'
#' # When not found
#' get_itis("howdy")
#' get_itis(c("Chironomus riparius", "howdy"))
#'
#' # Using common names
#' get_itis("black bear", searchtype="common")
#'
#' # Convert a tsn without class information to a itis class
#' as.itis(get_itis("Quercus douglasii")) # already a itis, returns the same
#' as.itis(get_itis(c("Chironomus riparius","Pinus contorta"))) # same
#' as.itis(19322) # numeric
#' as.itis(c(19322,129313,506198)) # numeric vector, length > 1
#' as.itis("19322") # character
#' as.itis(c("19322","129313","506198")) # character vector, length > 1
#' as.itis(list("19322","129313","506198")) # list, either numeric or character
#' ## dont check, much faster
#' as.itis("19322", check=FALSE)
#' as.itis(19322, check=FALSE)
#' as.itis(c("19322","129313","506198"), check=FALSE)
#' as.itis(list("19322","129313","506198"), check=FALSE)
#'
#' (out <- as.itis(c(19322,129313,506198)))
#' data.frame(out)
#' as.itis( data.frame(out) )
#'
#' # Get all data back
#' get_itis_("Arni")
#' get_itis_("Arni", rows=1)
#' get_itis_("Arni", rows=1:2)
#' get_itis_(c("asdfadfasd","Pinus contorta"), rows=1:5)
#' }

get_itis <- function(sci_com, searchtype = "scientific", accepted = FALSE,
  ask = TRUE, messages = TRUE, rows = NA, searchterm = NULL, ...) {

  assert(sci_com, c("character", "taxon_state"))
  assert(ask, "logical")
  assert(messages, "logical")
  assert(searchtype, "character")
  assert(accepted, "logical")
  assert_rows(rows)
  pchk(searchterm, "sci_com")

  if (inherits(sci_com, "character")) {
    tstate <- taxon_state$new(class = "itis", names = sci_com)
    items <- sci_com
  } else {
    assert_state(sci_com, "itis")
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

    searchtype <- match.arg(searchtype, c("scientific", "common"))
    tsn_df <- ritis::terms(sci_com[i], what = searchtype)
    mm <- NROW(tsn_df) > 1

    if (!inherits(tsn_df, "tbl_df") || NROW(tsn_df) == 0) {
      tsn <- NA_character_
      att <- "not found"
    } else {
      tsn_df$rank <- il_tsn2rank(tsn_df$tsn)
      # tsn_df$rank <- unlist(lapply(tsn_df$tsn, function(w) {
      #   tmp <- ritis::rank_name(w)
      #   if (NROW(tmp) == 0) NA_character_ else tolower(tmp$rankname)
      # }))

      if ("commonNames" %in% names(tsn_df)) {
        tsn_df$commonNames <-
          sapply(tsn_df$commonNames, function(z) paste0(z, collapse = ","))
      }

      tsn_df <- tsn_df[, c("tsn", "scientificName", "commonNames",
        "nameUsage", "rank")]

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
        rank_taken <- tsn_df$rank
        name <- tsn_df$scientificName
        att <- "found"
      }

      # check for direct match
      if (NROW(tsn_df) > 1) {
        tsn_df <- data.frame(tsn_df, stringsAsFactors = FALSE)
        names(tsn_df)[grep(searchtype, names(tsn_df))] <- "target"
        matchtmp <- tsn_df[tolower(tsn_df$target) %in% tolower(sci_com[i]), ]
        if (NROW(matchtmp) == 1) {
          tsn <- matchtmp$tsn
          rank_taken <- matchtmp$rank
          name <- matchtmp$target
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
          message("\nMore than one TSN found for taxon '", sci_com[i], "'!\n
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
            rank_taken <- tsn_df$rank[take]
            name <- tsn_df$target[take]
            att <- "found"
          } else {
            tsn <- NA_character_
            mssg(messages, "\nReturned 'NA'!\n\n")
            att <- "not found"
          }
        } else {
          if (length(tsn) != 1) {
            warning(sprintf(m_more_than_one_found, "tsn", sci_com[i]),
              call. = FALSE)
            tsn <- NA_character_
            att <- m_na_ask_false
          }
        }
      }

    }

    res <- list(id = as.character(tsn), name = name, rank = rank_taken,
      att = att, multiple = mm, direct = direct)
    prog$completed(sci_com[i], att)
    prog$prog(att)
    tstate$add(sci_com[i], res)
  }
  out <- tstate$get()
  ids <- as.character(unlist(pluck(out, "id")))
  res <- taxa_taxon(
    name = unlist(pluck(out, "name")),
    id = taxa::taxon_id(ids, db = "itis"),
    rank = unlist(pluck(out, "rank")),
    uri = sprintf(get_url_templates$itis, ids),
    match = unname(unlist(pluck(out, "att"))),
    multiple_matches = unname(unlist(pluck(out, "multiple"))),
    pattern_match = unname(unlist(pluck(out, "direct"))),
    class = "itis"
  )
  on.exit(prog$prog_summary(), add = TRUE)
  on.exit(tstate$exit, add = TRUE)
  return(res)
}
#' @export
#' @rdname get_itis
get_tsn <- get_itis

#' @export
#' @rdname get_itis
as.itis <- function(x, check=TRUE) UseMethod("as.itis")

#' @export
#' @rdname get_itis
as.itis.itis <- function(x, check=TRUE) x

#' @export
#' @rdname get_itis
as.itis.character <- function(x, check=TRUE) if (length(x) == 1) make_itis(x, check) else collapse(x, make_itis, "itis", check = check)

#' @export
#' @rdname get_itis
as.itis.list <- function(x, check=TRUE) if (length(x) == 1) make_itis(x, check) else collapse(x, make_itis, "itis", check = check)

#' @export
#' @rdname get_itis
as.itis.numeric <- function(x, check=TRUE) as.itis(as.character(x), check)

#' @export
#' @rdname get_itis
as.itis.data.frame <- function(x, check=TRUE) as_txid_df(x, check)

make_itis <- function(x, check=TRUE) {
  make_generic(x, get_url_templates$itis, "itis", check)
}

check_itis <- function(x){
  tt <- suppressMessages(itis_getrecord(x))
  identical(tt$acceptedNameList$tsn, as.character(x))
}

#' @export
#' @rdname get_itis
get_itis_ <- function(sci_com, messages = TRUE, searchtype = "scientific",
                     accepted = TRUE, rows = NA, searchterm = NULL, ...) {
  pchk(searchterm, "sci_com")
  stats::setNames(
    lapply(sci_com, get_itis_help, messages = messages,
           searchtype = searchtype, accepted = accepted, rows = rows, ...),
    sci_com
  )
}
#' @export
#' @rdname get_itis
get_tsn_ <- get_itis_

get_itis_help <- function(sci_com, messages, searchtype, accepted,
  rows, ...) {

  mssg(messages, "\nRetrieving data for taxon '", sci_com, "'\n")
  searchtype <- match.arg(searchtype, c("scientific", "common"))
  df <- ritis::terms(sci_com, what = searchtype, ...)
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
