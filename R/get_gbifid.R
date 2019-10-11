#' Get the GBIF backbone taxon ID from taxonomic names.
#'
#' @export
#' @param sciname (character) one or more scientific names. Or, a `taxon_state`
#' object (see [taxon-state])
#' @param ask logical; should get_colid be run in interactive mode?
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
#' @param rank (character) A taxonomic rank name. See [rank_ref()] for possible
#' options. Though note that some data sources use atypical ranks, so inspect the
#' data itself for options. Optional. See `Filtering` below.
#' @param method (character) one of "backbone" or "lookup". See Details.
#' @param x Input to [as.gbifid()]
#' @param check logical; Check if ID matches any existing on the DB, only used in
#' [as.gbifid()]
#' @param ... Ignored
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
#' # the new way
#' x <- get_gbifid("Puma concolor")
#' x
#' x[[1]]
#' x[[1]]$name
#' x[[1]]$rank
#' x[[1]]$attributes
#' x[[1]]$is_empty()
#' if (interactive()) x[[1]]$browse()
#
#' get_gbifid(sciname='Poa annua')
#' get_gbifid(sciname='Pinus contorta')
#' get_gbifid(sciname='Puma concolor')
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
#' get_gbifid(sciname='Pinus')
#' get_gbifid(sciname='Pinus', rows=10)
#' get_gbifid(sciname='Pinus', rows=1:3)
#'
#' # When not found, NA given
#' get_gbifid(sciname="uaudnadndj")
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
#' # Convert a gbifid without class information to a gbifid class
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
#' as.gbifid(c("2704179","2435099","3171445"), check=FALSE)
#' as.gbifid(list("2704179","2435099","3171445"), check=FALSE)
#'
#' (out <- as.gbifid(c(2704179,2435099,3171445)))
#' data.frame(out)
#' as.gbifid( data.frame(out) )
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

get_gbifid <- function(sciname, ask = TRUE, messages = TRUE, rows = NA,
                       phylum = NULL, class = NULL, order = NULL,
                       family = NULL, rank = NULL, method = "backbone", 
                       ...) {

  assert(sciname, c("character", "taxon_state"))
  assert(ask, "logical")
  assert(messages, "logical")
  assert(phylum, "character")
  assert(class, "character")
  assert(order, "character")
  assert(family, "character")
  assert(rank, "character")
  assert(method, "character")
  assert_rows(rows)

  if (inherits(sciname, "character")) {
    tstate <- taxon_state$new(class = "gbifid", names = sciname)
    items <- sciname
  } else {
    assert_state(sciname, "gbifid")
    tstate <- sciname
    sciname <- tstate$taxa_remaining()
    items <- c(sciname, tstate$taxa_completed())
  }

  prog <- progressor$new(items = items, suppress = !messages)
  done <- tstate$get()
  for (i in seq_along(done)) prog$completed(names(done)[i], done[[i]]$att)
  prog$prog_start()

  for (i in seq_along(sciname)) {
    direct <- FALSE
    mssg(messages, "\nRetrieving data for taxon '", sciname[i], "'\n")
    df <- switch(
      method,
      backbone = gbif_name_backbone(sciname[i], ...),
      lookup = gbif_name_lookup(sciname[i], ...)
    )
    mm <- NROW(df) > 1

    if (is.null(df) || NROW(df) == 0) df <- data.frame(NULL)

    if (nrow(df) == 0) {
      mssg(messages, m_not_found_sp_altclass)
      id <- NA_character_
      rank_taken <- NA_character_
      name <- NA_character_
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
      matchtmp <- df[as.character(df$canonicalname) %in% sciname[i], ]
      if (length(matchtmp) == 1) {
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
            att <- "found"
            name <- df$canonicalname
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
            message("\nMore than one GBIF ID found for taxon '", sciname[i], "'!\n
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
              message("Input accepted, took gbifid '",
                      as.character(df$gbifid[take]), "'.\n")
              id <- as.character(df$gbifid[take])
              name <- df$scientificname[take] %||% df$canonicalname[take]
              rank_taken <- df$rank[take]
              att <- "found"
            } else {
              id <- NA_character_
              att <- "not found"
              name <- NA_character_
              rank_taken <- NA_character_
              mssg(messages, "\nReturned 'NA'!\n\n")
            }
          } else {
            if (length(id) != 1) {
              warning(sprintf(m_more_than_one_found, "gbifid", sciname[i]),
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
    res <- list(id = id, name = name, rank = rank_taken,
      att = att, multiple = mm, direct = direct)
    prog$completed(sciname[i], att)
    prog$prog(att)
    tstate$add(sciname[i], res)
  }

  out <- tstate$get()
  res <- taxa::taxa(.list = lapply(out, function(z) {
    if (is.na(z$id)) {
      out <- taxa::taxon(NULL)
    } else {
      url <- sprintf(get_url_templates$gbif, z$id)
      out <- taxa::taxon(
        taxa::taxon_name(z$name %||% "", taxa::database_list$gbif),
        taxa::taxon_rank(z$rank, taxa::database_list$gbif),
        taxa::taxon_id(z$id %||% "", taxa::database_list$gbif, url)
      )
    }
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
#' @rdname get_gbifid
as.gbifid <- function(x, check=FALSE) UseMethod("as.gbifid")

#' @export
#' @rdname get_gbifid
as.gbifid.gbifid <- function(x, check=FALSE) x

#' @export
#' @rdname get_gbifid
as.gbifid.Taxa <- function(x, check=FALSE) x

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
as.gbifid.data.frame <- as_class_data_frame("gbifid")

#' @export
#' @rdname get_gbifid
as.data.frame.gbifid <- as_data_frame_general

make_gbifid <- function(x, check=TRUE) make_generic(x, 'https://www.gbif.org/species/%s', "gbifid", check)

check_gbifid <- function(x){
  tryid <- tryCatch(gbif_name_usage(key = x), error = function(e) e)
  valid <- !inherits(tryid, "error") && !is.null(tryid$key)
  name <- tryid$canonicalName
  rank <- tolower(tryid$rank)
  structure(valid, name = name, rank = rank)
}

#' @export
#' @rdname get_gbifid
get_gbifid_ <- function(sciname, messages = TRUE, rows = NA, method = "backbone"){
  setNames(lapply(sciname, get_gbifd_help, messages = messages, rows = rows, method = method), sciname)
}

get_gbifd_help <- function(sciname, messages, rows, method){
  mssg(messages, "\nRetrieving data for taxon '", sciname, "'\n")
  df <- switch(
    method,
    backbone = gbif_name_backbone(sciname),
    lookup = gbif_name_lookup(sciname)
  )
  if (!is.null(df)) df <- nmslwr(df)
  sub_rows(df, rows)
}
