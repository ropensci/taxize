#' Get the GBIF backbone taxon ID from taxonomic names.
#'
#' @export
#' @param sci (character) one or more scientific names. Or, a `taxon_state`
#' object (see [taxon-state])
#' @param ask logical; should get_gbifid be run in interactive mode?
#' If TRUE and more than one ID is found for the species, the user is asked for
#' input. If FALSE NA is returned for multiple matches.
#' @param messages logical; If TRUE the actual taxon queried is printed on the console.
#' @param rows numeric; Any number from 1 to infinity. If the default NA, all rows are considered.
#' Note that this function still only gives back a gbifid class object with one to many identifiers.
#' See [get_gbifid_()] to get back all, or a subset, of the raw data that you are
#' presented during the ask process.
#' @param phylum (character) A phylum (aka division) name. Optional. See `Filtering`
#' below.
#' @param class (character) A class name. Optional. See `Filtering` below.
#' @param order (character) An order name. Optional. See `Filtering` below.
#' @param family (character) A family name. Optional. See `Filtering` below.
#' @param rank (character) A taxonomic rank name. See [rank_ref] for possible
#' options. Though note that some data sources use atypical ranks, so inspect the
#' data itself for options. Optional. See `Filtering` below.
#' @param method (character) one of "backbone" or "lookup". See Details.
#' @param x Input to [as.gbifid()]
#' @param check logical; Check if ID matches any existing on the DB, only used in
#' [as.gbifid()]
#' @param ... Ignored
#' @param sciname Deprecated, see `sci`
#' @template getreturn
#'
#' @family taxonomic-ids
#' @seealso [classification()]
#'
#' @author Scott Chamberlain, 
#'
#' @details Internally in this function we use a function to search GBIF's taxonomy,
#' and if we find an exact match we return the ID for that match. If there isn't an
#' exact match we return the options to you to pick from.
#'
#' @section method parameter:
#' "backbone" uses the `/species/match` GBIF API route, matching against their
#' backbone taxonomy. We turn on fuzzy matching by default, as the search without
#' fuzzy against backbone is quite narrow. "lookup" uses the `/species/search`
#' GBIF API route, doing a full text search of name usages covering scientific
#' and vernacular named, species descriptions, distributions and the entire
#' classification.
#'
#' @section Filtering:
#' The parameters `phylum`, `class`, `order`, `family`, and `rank` are not used
#' in the search to the data provider, but are used in filtering the data down
#' to a subset that is closer to the target you want.  For all these parameters,
#' you can use regex strings since we use [grep()] internally to match.
#' Filtering narrows down to the set that matches your query, and removes the rest.
#'
#' @examples \dontrun{
#' get_gbifid(sci='Poa annua')
#' get_gbifid(sci='Pinus contorta')
#' get_gbifid(sci='Puma concolor')
#' 
#' #lots of queries
#' spp <- names_list("species", 10)
#' res <- get_gbifid(spp)
#' res
#' xx <- taxon_last()
#' xx
#'
#' # multiple names
#' get_gbifid(c("Poa annua", "Pinus contorta"))
#'
#' # specify rows to limit choices available
#' get_gbifid(sci='Pinus')
#' get_gbifid(sci='Pinus', rows=10)
#' get_gbifid(sci='Pinus', rows=1:3)
#'
#' # When not found, NA given
#' get_gbifid(sci="uaudnadndj")
#' get_gbifid(c("Chironomus riparius", "uaudnadndj"))
#'
#' # Narrow down results to a division or rank, or both
#' ## Satyrium example
#' ### Results w/o narrowing
#' get_gbifid("Satyrium")
#' ### w/ phylum
#' get_gbifid("Satyrium", phylum = "Tracheophyta")
#' get_gbifid("Satyrium", phylum = "Arthropoda")
#' ### w/ phylum & rank
#' get_gbifid("Satyrium", phylum = "Arthropoda", rank = "genus")
#'
#' ## Rank example
#' get_gbifid("Poa", method = "lookup")
#' get_gbifid("Poa", method = "lookup", rank = "genus")
#' get_gbifid("Poa", method = "lookup", family = "Thripidae")
#'
#' # Fuzzy filter on any filtering fields
#' ## uses grep on the inside
#' get_gbifid("Satyrium", phylum = "arthropoda")
#' get_gbifid("A*", method = "lookup", order = "*tera")
#' get_gbifid("A*", method = "lookup", order = "*ales")
#'
#' # Convert a uid without class information to a uid class
#' as.gbifid(get_gbifid("Poa annua")) # already a uid, returns the same
#' as.gbifid(get_gbifid(c("Poa annua","Puma concolor"))) # same
#' as.gbifid(2704179) # numeric
#' as.gbifid(c(2704179,2435099,3171445)) # numeric vector, length > 1
#' as.gbifid("2704179") # character
#' as.gbifid(c("2704179","2435099","3171445")) # character vector, length > 1
#' as.gbifid(list("2704179","2435099","3171445")) # list, either numeric or character
#' ## dont check, much faster
#' as.gbifid("2704179", check=FALSE)
#' as.gbifid(2704179, check=FALSE)
#' as.gbifid(2704179, check=FALSE)
#' as.gbifid(c("2704179","2435099","3171445"), check=FALSE)
#' as.gbifid(list("2704179","2435099","3171445"), check=FALSE)
#'
#' (out <- as.gbifid(c(2704179,2435099,3171445)))
#' data.frame(out)
#' as.uid( data.frame(out) )
#'
#' # Get all data back
#' get_gbifid_("Puma concolor")
#' get_gbifid_(c("Pinus", "uaudnadndj"))
#' get_gbifid_(c("Pinus", "Puma"), rows=5)
#' get_gbifid_(c("Pinus", "Puma"), rows=1:5)
#'
#' # use curl options
#' invisible(get_gbifid("Quercus douglasii", verbose = TRUE))
#' }

get_gbifid <- function(sci, ask = TRUE, messages = TRUE, rows = NA,
                       phylum = NULL, class = NULL, order = NULL,
                       family = NULL, rank = NULL, method = "backbone",
                       sciname = NULL, ...) {

  assert(sci, c("character", "taxon_state"))
  assert(ask, "logical")
  assert(messages, "logical")
  assert(phylum, "character")
  assert(class, "character")
  assert(order, "character")
  assert(family, "character")
  assert(rank, "character")
  assert(method, "character")
  assert_rows(rows)
  
  if (!is.null(sciname)) {
    lifecycle::deprecate_warn(when = "v0.9.97", what = "get_gbifid(sciname)", with = "get_gbifid(sci)")
    sci <- sciname
  }
  
  if (inherits(sci, "character")) {
    tstate <- taxon_state$new(class = "gbifid", names = sci)
    items <- sci
  } else {
    assert_state(sci, "gbifid")
    tstate <- sci
    sci <- tstate$taxa_remaining()
    items <- c(sci, tstate$taxa_completed())
  }

  # Escape problematic characters
  sci <- gsub(sci, pattern = "[^\\]?'", replacement = "\\\\'")
  
  prog <- progressor$new(items = items, suppress = !messages)
  done <- tstate$get()
  for (i in seq_along(done)) prog$completed(names(done)[i], done[[i]]$att)
  prog$prog_start()

  for (i in seq_along(sci)) {
    direct <- FALSE
    mssg(messages, "\nRetrieving data for taxon '", sci[i], "'\n")
    df <- switch(
      method,
      backbone = gbif_name_backbone(sci[i], ...),
      lookup = gbif_name_lookup(sci[i], ...)
    )
    mm <- NROW(df) > 1

    if (is.null(df)) df <- data.frame(NULL)

    if (nrow(df) == 0) {
      mssg(messages, m_not_found_sp_altclass)
      id <- NA_character_
      att <- "not found"
    } else {
      names(df)[1] <- "gbifid"
      id <- df$gbifid
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
      matchtmp <- df[as.character(df$canonicalname) %in% sci[i], "gbifid"]
      if (length(matchtmp) == 1) {
        id <- as.character(matchtmp)
        direct <- TRUE
      } else {
        if (!is.null(phylum) || !is.null(class) || !is.null(order) ||
            !is.null(family) || !is.null(rank)) {
          df <- filt(df, "phylum", phylum)
          df <- filt(df, "class", class)
          df <- filt(df, "order", order)
          df <- filt(df, "family", family)
          df <- filt(df, "rank", rank)
        }

        df <- sub_rows(df, rows)
        if (NROW(df) == 0) {
          id <- NA_character_
          att <- "not found"
        } else {
          id <- df$gbifid
          if (length(id) == 1) {
            rank_taken <- as.character(df$rank)
            att <- "found"
          }
        }

        # more than one found -> user input
        if (length(id) > 1) {
          if (ask) {
            # limit to subset of columns for ease of use
            df <- df[, switch(method,
                              backbone = gbif_cols_show_backbone,
                              lookup = gbif_cols_show_lookup)]

            # prompt
            message("\n\n")
            message("\nMore than one GBIF ID found for taxon '", sci[i], "'!\n
            Enter rownumber of taxon (other inputs will return 'NA'):\n")
            rownames(df) <- 1:nrow(df)
            message(paste0(utils::capture.output(df), collapse = "\n"))
            take <- scan(n = 1, quiet = TRUE, what = 'raw')

            if (length(take) == 0) {
              take <- 'notake'
              att <- 'nothing chosen'
            }
            if (take %in% seq_len(nrow(df))) {
              take <- as.numeric(take)
              message("Input accepted, took gbifid '",
                      as.character(df$gbifid[take]), "'.\n")
              id <- as.character(df$gbifid[take])
              att <- "found"
            } else {
              id <- NA_character_
              att <- "not found"
              mssg(messages, "\nReturned 'NA'!\n\n")
            }
          } else {
            if (length(id) != 1) {
              warning(sprintf(m_more_than_one_found, "gbifid", sci[i]),
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
  ids <- structure(as.character(unlist(pluck(out, "id"))), class = "gbifid",
                   match = pluck_un(out, "att", ""),
                   multiple_matches = pluck_un(out, "multiple", logical(1)),
                   pattern_match = pluck_un(out, "direct", logical(1)))
  on.exit(prog$prog_summary(), add = TRUE)
  on.exit(tstate$exit, add = TRUE)
  add_uri(ids, get_url_templates$gbif)
}

#' @export
#' @rdname get_gbifid
as.gbifid <- function(x, check=FALSE) UseMethod("as.gbifid")

#' @export
#' @rdname get_gbifid
as.gbifid.gbifid <- function(x, check=FALSE) x

#' @export
#' @rdname get_gbifid
as.gbifid.character <- function(x, check=TRUE) if(length(x) == 1) make_gbifid(x, check) else collapse(x, make_gbifid, "gbifid", check=check)

#' @export
#' @rdname get_gbifid
as.gbifid.list <- function(x, check=TRUE) if(length(x) == 1) make_gbifid(x, check) else collapse(x, make_gbifid, "gbifid", check=check)

#' @export
#' @rdname get_gbifid
as.gbifid.numeric <- function(x, check=TRUE) as.gbifid(as.character(x), check)

#' @export
#' @rdname get_gbifid
as.gbifid.data.frame <- function(x, check = TRUE) {
  structure(x$ids, class = "gbifid", match = x$match,
            multiple_matches = x$multiple_matches,
            pattern_match = x$pattern_match, uri = x$uri)
}

#' @export
#' @rdname get_gbifid
as.data.frame.gbifid <- function(x, ...){
  data.frame(ids = as.character(unclass(x)),
             class = "gbifid",
             match = attr(x, "match"),
             multiple_matches = attr(x, "multiple_matches"),
             pattern_match = attr(x, "pattern_match"),
             uri = attr(x, "uri"),
             stringsAsFactors = FALSE)
}

make_gbifid <- function(x, check=TRUE) make_generic(x, 'https://www.gbif.org/species/%s', "gbifid", check)

check_gbifid <- function(x){
  tryid <- tryCatch(gbif_name_usage(key = x), error = function(e) e)
  if ( "error" %in% class(tryid) && is.null(tryid$key) ) FALSE else TRUE
}

#' @export
#' @rdname get_gbifid
get_gbifid_ <- function(sci, messages = TRUE, rows = NA, method = "backbone",
  sciname = NULL) {

  if (!is.null(sciname)) {
    lifecycle::deprecate_warn(when = "v0.9.97", what = "get_gbifid_(sciname)", with = "get_gbifid_(sci)")
    sci <- sciname
  }
  stats::setNames(lapply(sci, get_gbifd_help, messages = messages,
    rows = rows, method = method), sci)
}

get_gbifd_help <- function(sci, messages, rows, method){
  mssg(messages, "\nRetrieving data for taxon '", sci, "'\n")
  df <- switch(
    method,
    backbone = gbif_name_backbone(sci),
    lookup = gbif_name_lookup(sci)
  )
  if (!is.null(df)) df <- nmslwr(df)
  sub_rows(df, rows)
}
