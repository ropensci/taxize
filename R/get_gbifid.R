#' Get the GBIF backbone taxon ID from taxonomic names.
#'
#' @export
#' @param sci (character) one or more scientific names. Or, a `taxon_state`
#' object (see [taxon-state])
#' @param ask logical; should get_gbif be run in interactive mode?
#' If TRUE and more than one ID is found for the species, the user is asked for
#' input. If FALSE NA is returned for multiple matches.
#' @param messages logical; If TRUE the actual taxon queried is printed on the console.
#' @param rows numeric; Any number from 1 to infinity. If the default NA, all rows are considered.
#' Note that this function still only gives back a gbifid class object with one to many identifiers.
#' See [get_gbif_()] to get back all, or a subset, of the raw data that you are
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
#' @param x Input to [as.gbif()]
#' @param check logical; Check if ID matches any existing on the DB, only used in
#' [as.gbif()]
#' @param ... Ignored
#' @param sciname Deprecated, see `sci`
#' @template getreturn
#'
#' @family taxonomic-ids
#' @seealso [classification()]
#'
#' @author Scott Chamberlain, \email{myrmecocystus@@gmail.com}
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
#' get_gbif(sci='Poa annua')
#' get_gbif(sci='Pinus contorta')
#' get_gbif(sci='Puma concolor')
#' 
#' #lots of queries
#' spp <- names_list("species", 10)
#' res <- get_gbif(spp)
#' res
#' xx <- taxon_last()
#' xx
#'
#' # multiple names
#' x <- get_gbif(c("Poa annua", "Pinus contorta", "adf"))
#' x
#' as.data.frame(x) # returns a tibble
#' nms <- c("Poa", "Abies magnifica", "Pinaceae", "Pinopsida", "Eukaryota", "Abies")
#' w <- get_gbif(nms)
#' w
#' as.data.frame(w)
#' class(w)
#' 
#' # extract parts (maintains the txid/taxa_taxon class)
#' library(taxa2)
#' tax_rank(w)
#' as.character(tax_rank(w))
#' tax_id(w)
#' as.character(tax_id(w))
#' tax_name(w)
#' as.character(tax_name(w))
#' 
#' # subset (maintains the txid/taxa_taxon class)
#' w[1]
#' w[1:3]
#' w[tax_rank(w) > 'genus']
#' names(w) <- letters[1:6]
#' w
#' w[c('b', 'c')]
#' 
#' ## convert to taxonomy object
#' taxonomy(w)
#' 
#'
#' # specify rows to limit choices available
#' get_gbif(sci='Pinus')
#' get_gbif(sci='Pinus', rows=10)
#' get_gbif(sci='Pinus', rows=1:3)
#'
#' # When not found, NA given
#' get_gbif(sci="uaudnadndj")
#' get_gbif(c("Chironomus riparius", "uaudnadndj"))
#'
#' # Narrow down results to a division or rank, or both
#' ## Satyrium example
#' ### Results w/o narrowing
#' get_gbif("Satyrium")
#' ### w/ phylum
#' get_gbif("Satyrium", phylum = "Tracheophyta")
#' get_gbif("Satyrium", phylum = "Arthropoda")
#' ### w/ phylum & rank
#' get_gbif("Satyrium", phylum = "Arthropoda", rank = "genus")
#'
#' ## Rank example
#' get_gbif("Poa", method = "lookup")
#' get_gbif("Poa", method = "lookup", rank = "genus")
#' get_gbif("Poa", method = "lookup", family = "Thripidae")
#'
#' # Fuzzy filter on any filtering fields
#' ## uses grep on the inside
#' get_gbif("Satyrium", phylum = "arthropoda")
#' get_gbif("A*", method = "lookup", order = "*tera")
#' get_gbif("A*", method = "lookup", order = "*ales")
#'
#' # Convert a gbifid without class information to a gbifid class
#' as.gbif(get_gbif("Poa annua")) # already a gbifid, returns the same
#' as.gbif(get_gbif(c("Poa annua","Puma concolor"))) # same
#' as.gbif(2704179) # numeric
#' as.gbif(c(2704179,2435099,3171445)) # numeric vector, length > 1
#' as.gbif("2704179") # character
#' as.gbif(c("2704179","2435099","3171445")) # character vector, length > 1
#' as.gbif(list("2704179","2435099","3171445")) # list, either numeric or character
#' ## dont check, much faster
#' as.gbif("2704179", check=FALSE)
#' as.gbif(2704179, check=FALSE)
#' as.gbif(2704179, check=FALSE)
#' as.gbif(c("2704179","2435099","3171445"), check=FALSE)
#' as.gbif(list("2704179","2435099","3171445"), check=FALSE)
#'
#' (out <- as.gbif(c(2704179,2435099,3171445)))
#' data.frame(out)
#' as.gbif( data.frame(out) )
#'
#' # Get all data back
#' get_gbif_("Puma concolor")
#' get_gbif_(c("Pinus", "uaudnadndj"))
#' get_gbif_(c("Pinus", "Puma"), rows=5)
#' get_gbif_(c("Pinus", "Puma"), rows=1:5)
#'
#' # use curl options
#' invisible(get_gbif("Quercus douglasii", verbose = TRUE))
#' }

get_gbif <- function(sci, ask = TRUE, messages = TRUE, rows = NA,
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
  pchk(sciname, "sci")

  if (inherits(sci, "character")) {
    tstate <- taxon_state$new(class = "gbif", names = sci)
    items <- sci
  } else {
    assert_state(sci, "gbif")
    tstate <- sci
    sci <- tstate$taxa_remaining()
    items <- c(sci, tstate$taxa_completed())
  }

  prog <- progressor$new(items = items, suppress = !messages)
  done <- tstate$get()
  for (i in seq_along(done)) prog$completed(names(done)[i], done[[i]]$att)
  prog$prog_start()

  for (i in seq_along(sci)) {
    name <- NA_character_
    rank_taken <- NA_character_
    direct <- FALSE
    mssg(messages, "\nRetrieving data for taxon '", sci[i], "'\n")
    df <- switch(
      method,
      backbone = gbif_name_backbone(sci[i], ...),
      lookup = gbif_name_lookup(sci[i], ...)
    )
    mm <- NROW(df) > 1

    if (is.null(df) || NROW(df) == 0) df <- data.frame(NULL)

    if (NROW(df) == 0) {
      mssg(messages, m_not_found_sp_altclass)
      id <- NA_character_
      att <- "not found"
    } else {
      names(df)[1] <- "gbifid"
      id <- df$gbifid
      name <- df$canonicalname
      rank_taken <- df$rank
      att <- "found"
    }

    # not found
    if (length(id) == 0) {
      mssg(messages, m_not_found_sp_altclass)
      id <- NA_character_
      name <- NA_character_
      rank_taken <- NA_character_
      att <- "not found"
    }

    if (length(id) > 1) {
      # check for exact match
      matchtmp <- df[as.character(df$canonicalname) %in% sci[i], ]
      if (length(matchtmp) == 1) {
        # id <- as.character(matchtmp)
        id <- as.character(matchtmp$gbifid)
        name <- matchtmp$canonicalname
        rank_taken <- matchtmp$rank
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
          name <- NA_character_
          rank_taken <- NA_character_
          att <- "not found"
        } else {
          id <- df$gbifid
          if (length(id) == 1) {
            rank_taken <- as.character(df$rank)
            name <- df$canonicalname
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
            print(df)
            take <- scan(n = 1, quiet = TRUE, what = 'raw')

            if (length(take) == 0) {
              take <- 'notake'
              att <- 'nothing chosen'
            }
            if (take %in% seq_len(nrow(df))) {
              take <- as.numeric(take)
              message("Input accepted, took GBIF ID '",
                      as.character(df$gbifid[take]), "'.\n")
              id <- as.character(df$gbifid[take])
              name <- df$scientificname[take] %||% df$canonicalname[take]
              rank_taken <- df$rank[take]
              att <- "found"
            } else {
              id <- NA_character_
              name <- NA_character_
              rank_taken <- NA_character_
              att <- "not found"
              mssg(messages, "\nReturned 'NA'!\n\n")
            }
          } else {
            if (length(id) != 1) {
              warning(sprintf(m_more_than_one_found, "GBIF ID", sci[i]),
                call. = FALSE)
              id <- NA_character_
              name <- NA_character_
              rank_taken <- NA_character_
              att <- m_na_ask_false
            }
          }
        }
      }
    }
    res <- list(id = id, name = name, rank = rank_taken, att = att,
      multiple = mm, direct = direct)
    prog$completed(sci[i], att)
    prog$prog(att)
    tstate$add(sci[i], res)
  }
  out <- tstate$get()
  res <- make_taxa_taxon(out, "gbif")
  on.exit(prog$prog_summary(), add = TRUE)
  on.exit(tstate$exit, add = TRUE)
  return(res)
}
#' @export
#' @rdname get_gbif
get_gbifid <- function(...) {
  fchk("get_gbifid", "get_gbif")
  get_gbif(...)
}

#' @export
#' @rdname get_gbif
as.gbif <- function(x, check=FALSE) UseMethod("as.gbif")

#' @export
#' @rdname get_gbif
as.gbif.txid <- function(x, check=FALSE) x

#' @export
#' @rdname get_gbif
as.gbif.character <- function(x, check=TRUE) if(length(x) == 1) make_gbif(x, check) else collapse(x, make_gbif, "gbif", check=check)

#' @export
#' @rdname get_gbif
as.gbif.list <- function(x, check=TRUE) if(length(x) == 1) make_gbif(x, check) else collapse(x, make_gbif, "gbif", check=check)

#' @export
#' @rdname get_gbif
as.gbif.numeric <- function(x, check=TRUE) as.gbif(as.character(x), check)

#' @export
#' @rdname get_gbif
as.gbif.data.frame <- function(x, check = TRUE) as_txid_df(x, check)

make_gbif <- function(x, check=TRUE) make_generic(x, 'https://www.gbif.org/species/%s', "gbif", check)

check_gbif <- function(x){
  tryid <- tryCatch(gbif_name_usage(key = x), error = function(e) e)
  if ( "error" %in% class(tryid) && is.null(tryid$key) ) FALSE else TRUE
}

#' @export
#' @rdname get_gbif
get_gbif_ <- function(sci, messages = TRUE, rows = NA, method = "backbone",
  sciname = NULL) {

  pchk(sciname, "sci")
  stats::setNames(lapply(sci, get_gbifd_help, messages = messages,
    rows = rows, method = method), sci)
}
#' @export
#' @rdname get_gbif
get_gbifid_ <- function(...) {
  fchk("get_gbifid_", "get_gbif_")
  get_gbif_(...)
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
