#' Get the UID codes from NCBI for taxonomic names.
#'
#' Retrieve the Unique Identifier (UID) of a taxon from NCBI taxonomy browser.
#'
#' @export
#' @param sci_com character; scientific or common name. Or, a `taxon_state`
#' object (see [taxon-state])
#' @param ask logical; should get_uid be run in interactive mode? If TRUE and
#' more than one TSN is found for the species, the user is asked for input. If
#' FALSE NA is returned for multiple matches.
#' @param messages logical; If `TRUE` (default) the actual taxon queried is
#' printed on the console.
#' @param rows numeric; Any number from 1 to infinity. If the default NA, all
#' rows are considered. Note that this function still only gives back a uid
#' class object with one to many identifiers. See [get_uid_()] to get back
#' all, or a subset, of the raw data that you are presented during the ask
#' process.
#' @param modifier (character) A modifier to the `sci_com` given. Options
#' include: Organism, Scientific Name, Common Name, All Names, Division,
#' Filter, Lineage, GC, MGC, Name Tokens, Next Level, PGC, Properties, Rank,
#' Subtree, Synonym, Text Word. These are not checked, so make sure they are
#' entered correctly, as is.
#' @param rank_query (character) A taxonomic rank name to modify the query sent
#' to NCBI. See [rank_ref] for possible options. Though note that
#' some data sources use atypical ranks, so inspect the data itself for
#' options. Optional. See `Querying` below.
#' @param division_filter (character) A division (aka phylum) name to filter
#' data after retrieved from NCBI. Optional. See `Filtering` below.
#' @param rank_filter (character) A taxonomic rank name to filter data after
#' retrieved from NCBI. See [rank_ref] for possible options.
#' Though note that some data sources use atypical ranks, so inspect the data
#' itself for options. Optional. See `Filtering` below.
#' @param key (character) NCBI Entrez API key. optional. See Details.
#' @param x Input to [as.uid()]
#' @param sciname Deprecated, see `sci_com`
#' @param ... Ignored
#' @param check logical; Check if ID matches any existing on the DB, only used
#'   in [as.uid()]
#' @template getreturn
#'
#' @section Querying: The parameter `rank_query` is used in the search sent
#'   to NCBI, whereas `rank_filter` filters data after it comes back. The
#'   parameter `modifier` adds modifiers to the name. For example,
#'   `modifier="Organism"` adds that to the name, giving e.g.,
#'   `Helianthus[Organism]`.
#'
#' @section Filtering: The parameters `division_filter` and
#'   `rank_filter` are not used in the search to the data provider, but are
#'   used in filtering the data down to a subset that is closer to the target
#'   you want. For all these parameters, you can use regex strings since we use
#'   [grep()] internally to match. Filtering narrows down to the set
#'   that matches your query, and removes the rest.
#'
#' @section Beware: NCBI does funny things sometimes. E.g., if you search on
#'   Fringella morel, a slight misspelling of the genus name, and a non-existent
#'   epithet, NCBI gives back a morel fungal species. In addition, NCBI doesn't
#'   really do fuzzy searching very well, so if there is a slight mis-spelling
#'   in your names, you likely won't get what you are expecting. The lesson:
#'   clean your names before using this function. Other data sources are better
#'   about fuzzy matching.
#'
#' @section Authentication:
#' See [taxize-authentication] for help on authentication
#'
#' Note that even though you can't pass in your key to `as.uid` functions,
#' we still use your Entrez API key if you have it saved as an R option
#' or environment variable.
#' 
#' @section HTTP version:
#' We hard code `http_version = 2L` to use HTTP/1.1 in HTTP requests to
#' the Entrez API. See `curl::curl_symbols('CURL_HTTP_VERSION')` 
#'
#' @family taxonomic-ids
#' @seealso [classification()]
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#'
#' @examples \dontrun{
#' get_uid(c("Chironomus riparius", "Chaetopteryx"))
#' get_uid(c("Chironomus riparius", "aaa vva"))
#'
#' # When not found
#' get_uid("howdy")
#' get_uid(c("Chironomus riparius", "howdy"))
#'
#' # Narrow down results to a division or rank, or both
#' ## By modifying the query
#' ### w/ modifiers to the name
#' get_uid(sci_com = "Aratinga acuticauda", modifier = "Organism")
#' get_uid(sci_com = "bear", modifier = "Common Name")
#'
#' ### w/ rank query
#' get_uid(sci_com = "Pinus", rank_query = "genus")
#' get_uid(sci_com = "Pinus", rank_query = "subgenus")
#' ### division query doesn't really work, for unknown reasons, so not available
#'
#' ## By filtering the result
#' ## Echinacea example
#' ### Results w/o narrowing
#' get_uid("Echinacea")
#' ### w/ division
#' get_uid(sci_com = "Echinacea", division_filter = "eudicots")
#' get_uid(sci_com = "Echinacea", division_filter = "sea urchins")
#'
#' ## Satyrium example
#' ### Results w/o narrowing
#' get_uid(sci_com = "Satyrium")
#' ### w/ division
#' get_uid(sci_com = "Satyrium", division_filter = "monocots")
#' get_uid(sci_com = "Satyrium", division_filter = "butterflies")
#'
#' ## Rank example
#' get_uid(sci_com = "Pinus")
#' get_uid(sci_com = "Pinus", rank_filter = "genus")
#' get_uid(sci_com = "Pinus", rank_filter = "subgenus")
#'
#' # Fuzzy filter on any filtering fields
#' ## uses grep on the inside
#' get_uid("Satyrium", division_filter = "m")
#'
#' # specify rows to limit choices available
#' get_uid('Dugesia') # user prompt needed
#' get_uid('Dugesia', rows=1) # 2 choices, so returns only 1 row, so no choices
#' get_uid('Dugesia', ask = FALSE) # returns NA for multiple matches
#'
#' # Go to a website with more info on the taxon
#' res <- get_uid("Chironomus riparius")
#' browseURL(attr(res, "uri"))
#'
#' # Convert a uid without class information to a uid class
#' as.uid(get_uid("Chironomus riparius")) # already a uid, returns the same
#' as.uid(get_uid(c("Chironomus riparius","Pinus contorta"))) # same
#' as.uid(315567) # numeric
#' as.uid(c(315567,3339,9696)) # numeric vector, length > 1
#' as.uid("315567") # character
#' as.uid(c("315567","3339","9696")) # character vector, length > 1
#' as.uid(list("315567","3339","9696")) # list, either numeric or character
#' ## dont check, much faster
#' as.uid("315567", check=FALSE)
#' as.uid(315567, check=FALSE)
#' as.uid(c("315567","3339","9696"), check=FALSE)
#' as.uid(list("315567","3339","9696"), check=FALSE)
#'
#' (out <- as.uid(c(315567,3339,9696)))
#' data.frame(out)
#' as.uid( data.frame(out) )
#'
#' # Get all data back
#' get_uid_("Puma concolor")
#' get_uid_("Dugesia")
#' get_uid_("Dugesia", rows=2)
#' get_uid_("Dugesia", rows=1:2)
#' get_uid_(c("asdfadfasd","Pinus contorta"))
#'
#' # use curl options
#' get_uid("Quercus douglasii", verbose = TRUE)
#' }

get_uid <- function(sci_com, ask = TRUE, messages = TRUE, rows = NA,
                    modifier = NULL, rank_query = NULL,
                    division_filter = NULL, rank_filter = NULL,
                    key = NULL, sciname = NULL, ...) {

  assert(sci_com, c("character", "taxon_state"))
  assert(ask, "logical")
  assert(messages, "logical")
  assert(modifier, "character")
  assert(rank_query, "character")
  assert(division_filter, "character")
  assert(rank_filter, "character")
  assert_rows(rows)
  key <- getkey(key, service = "entrez")
  pchk(sciname, "sci_com")

  if (inherits(sci_com, "character")) {
    tstate <- taxon_state$new(class = "uid", names = sci_com)
    items <- sci_com
  } else {
    assert_state(sci_com, "uid")
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
    mssg(messages, "\nRetrieving data for taxon '", sci_com[i], "'\n")
    sci_com[i] <- gsub(" ", "+", sci_com[i])
    if (!is.null(modifier))
      sci_com[i] <- paste0(sci_com[i], sprintf("[%s]", modifier))
    term <- sci_com[i]
    if (!is.null(rank_query))
      term <- paste0(term, sprintf(" AND %s[Rank]", rank_query))
    try_again_errors <- c("Could not resolve host: eutils.ncbi.nlm.nih.gov")
    query_args <- tc(list(db = "taxonomy", term = term, api_key = key))
    raw_xml_result <- repeat_until_it_works(try_again_errors,
                                            "esearch",
                                            query = query_args,
                                            ...)
    if (grepl("error", raw_xml_result)) stop(raw_xml_result, call.=FALSE)
    xml_result <- xml2::read_xml(raw_xml_result)

    # NCBI limits requests to three per second
    ncbi_rate_limit_pause(key)
    uid <- xml2::xml_text(xml2::xml_find_all(xml_result, "//IdList/Id"))
    mm <- length(uid) > 1

    if (length(uid) == 0) { # if taxon name is not found
      uid <- NA_character_
    } else {
      att <- 'found'
    }

    # not found on ncbi
    if (length(uid) == 0 || all(is.na(uid))) {
      mssg(messages, m_not_found_sp_altclass)
      uid <- NA_character_
      att <- 'NA due to not found'
    }
    # more than one found on ncbi -> user input
    if (length(uid) > 1) {
      ID <- paste(uid, collapse = ",")
      try_again_errors <- c("Could not resolve host: eutils.ncbi.nlm.nih.gov")
      query_args <- tc(list(db = "taxonomy", ID = ID, api_key = key))
      tt <- repeat_until_it_works(try_again_errors, "esummary",
                                  query_args, ...)
      ttp <- xml2::read_xml(tt)
      df <- parse_ncbi(ttp)
      rownames(df) <- 1:nrow(df)

      if (!is.null(division_filter) || !is.null(rank_filter)) {
        df <- filt(df, "division", division_filter)
        df <- filt(df, "rank", rank_filter)
      }

      df <- sub_rows(df, rows)
      uid <- df$uid
      if (length(uid) == 1) {
        direct <- TRUE
        att <- "found"
      }
      if (length(uid) == 0) {
        uid <- NA_character_
      }

      if (length(uid) > 1) {
        # check for exact match
        matchtmp <- df[
          tolower(
            as.character(df$scientificname)) %in% tolower(sci_com[i]), "uid"]
        if (length(matchtmp) == 1) {
          uid <- as.character(matchtmp)
          direct <- TRUE
        }
      }

      if (length(uid) > 1) {
        if (!ask) {
          if (length(uid) == 1) {
            att <- "found"
          } else {
            warning(
              sprintf(m_more_than_one_found, "UID", sci_com[i]),
              call. = FALSE
            )
            uid <- NA_character_
            att <- m_na_ask_false
          }
        } else {
          # prompt
          rownames(df) <- 1:nrow(df)
          message("\n\n")
          message("\nMore than one UID found for taxon '", sci_com[i], "'!\n
            Enter rownumber of taxon (other inputs will return 'NA'):\n")
          print(df)
          take <- scan(n = 1, quiet = TRUE, what = 'raw')

          if (length(take) == 0) {
            take <- "notake"
            att <- "nothing chosen"
          }
          if (take %in% seq_len(nrow(df))) {
            take <- as.numeric(take)
            message("Input accepted, took UID '",
                    as.character(df$uid[take]), "'.\n")
            uid <- as.character(df$uid[take])
            att <- 'found'
          } else {
            uid <- NA_character_
            att <- "NA due to user input out of range"
            mssg(messages, "\nReturned 'NA'!\n\n")
          }
        }
      }
    }
    res <- list(id = as.character(uid), att = att, multiple = mm,
      direct = direct)
    prog$completed(sci_com[i], att)
    prog$prog(att)
    tstate$add(sci_com[i], res)
  }
  out <- tstate$get()
  ids <- structure(pluck_un(out, "id", ""), class = "uid",
    match = pluck_un(out, "att", ""),
    multiple_matches = pluck_un(out, "multiple", logical(1)),
    pattern_match = pluck_un(out, "direct", logical(1))
  )
  on.exit(prog$prog_summary(), add = TRUE)
  on.exit(tstate$exit, add = TRUE)
  add_uri(ids, get_url_templates$ncbi)
}

repeat_until_it_works <- function(catch, path, query, max_tries = 3,
  wait_time = 10, messages = TRUE, ...) {

  error_handler <- function(e) {
    if (e$message %in% catch) {
      if (messages) warning(paste("Caught error:", e$message))
      return(NA)
    } else {
      stop(e$message)
    }
  }
  for (count in 1:max_tries) {
    cli <- crul::HttpClient$new(url = ncbi_base(),
      headers = tx_ual, opts = list(http_version = 2L, ...))
    res <- cli$get(sprintf("entrez/eutils/%s.fcgi", path),
      query = tc(query))
    output <- tryCatch(res$parse("UTF-8"), error = error_handler)
    if (!is.na(output)) return(output)
    Sys.sleep(wait_time * count)
  }
  return(output)
}

#' @export
#' @rdname get_uid
as.uid <- function(x, check=TRUE) UseMethod("as.uid")

#' @export
#' @rdname get_uid
as.uid.uid <- function(x, check=TRUE) x

#' @export
#' @rdname get_uid
as.uid.character <- function(x, check=TRUE) if(length(x) == 1) make_uid(x, check) else collapse(x, make_uid, "uid", check=check)

#' @export
#' @rdname get_uid
as.uid.list <- function(x, check=TRUE) if(length(x) == 1) make_uid(x, check) else collapse(x, make_uid, "uid", check=check)

#' @export
#' @rdname get_uid
as.uid.numeric <- function(x, check=TRUE) as.uid(as.character(x), check)

#' @export
#' @rdname get_uid
as.uid.data.frame <- function(x, check=TRUE) {
  structure(x$ids, class="uid", match=x$match,
            multiple_matches = x$multiple_matches,
            pattern_match = x$pattern_match, uri=x$uri)
}

#' @export
#' @rdname get_uid
as.data.frame.uid <- function(x, ...){
  data.frame(ids = as.character(unclass(x)),
             class = "uid",
             match = attr(x, "match"),
             multiple_matches = attr(x, "multiple_matches"),
             pattern_match = attr(x, "pattern_match"),
             uri = attr(x, "uri"),
             stringsAsFactors = FALSE)
}

make_uid <- function(x, check=TRUE) {
  make_generic(x, 'https://www.ncbi.nlm.nih.gov/taxonomy/%s',
    "uid", check)
}

check_uid <- function(x){
  key <- getkey(NULL, "ENTREZ_KEY")
  cli <- crul::HttpClient$new(url = ncbi_base(), headers = tx_ual,
    opts = list(http_version = 2L))
  args <- tc(list(db = "taxonomy", id = x, api_key = key))
  res <- cli$get("entrez/eutils/esummary.fcgi", query = args)
  res$raise_for_status()
  tt <- xml2::read_xml(res$parse("UTF-8"))
  tryid <- xml2::xml_text(xml2::xml_find_all(tt, "//Id"))
  identical(as.character(x), tryid)
}


#' @export
#' @rdname get_uid
get_uid_ <- function(sci_com, messages = TRUE, rows = NA, key = NULL,
  sciname = NULL, ...) {
  
  key <- getkey(key, "ENTREZ_KEY")
  pchk(sciname, "sci_com")
  stats::setNames(lapply(sci_com, get_uid_help, messages = messages,
    rows = rows, key = key, ...), sci_com)
}

get_uid_help <- function(sci_com, messages, rows, key, ...) {
  mssg(messages, "\nRetrieving data for taxon '", sci_com, "'\n")
  cli <- crul::HttpClient$new(url = ncbi_base(), headers = tx_ual,
    opts = list(http_version = 2L, ...))
  res <- cli$get(
    "entrez/eutils/esearch.fcgi",
    query = tc(list(api_key = key,
      db = "taxonomy", term = gsub(" ", "+", sci_com))))
  res$raise_for_status()
  xml_result <- xml2::read_xml(res$parse("UTF-8"))
  Sys.sleep(0.33)
  uid <- xml_text(xml_find_all(xml_result, "//IdList/Id"))
  if (length(uid) == 0) {
    NULL
  } else {
    res <- cli$get("entrez/eutils/esummary.fcgi",
      query = tc(list(api_key = key, db = "taxonomy",
        ID = paste(uid, collapse = ","))))
    res$raise_for_status()
    ttp <- xml2::read_xml(res$parse("UTF-8"))
    df <- parse_ncbi(ttp)
    sub_rows(df, rows)
  }
}

parse_ncbi <- function(x) {
  mget <- c("Status", "Rank", "Division", "ScientificName",
            "CommonName", "TaxId", "Genus", "Species", "Subsp",
            "ModificationDate")
  nget <- paste0('Item[@Name="', mget, "\"]")
  nodes <- xml_find_all(x, "//DocSum")
  tmp <- taxize_ldfast(lapply(nodes, function(z) {
    data.frame(as.list(
      setNames(sapply(nget, function(w) xml_text(xml_find_all(z, w))), tolower(mget))),
      stringsAsFactors = FALSE)
  }))
  rename(tmp, c('taxid' = 'uid'))
}

ncbi_base <- function() "https://eutils.ncbi.nlm.nih.gov"
