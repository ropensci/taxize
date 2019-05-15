#' Get the page name for a Wiki taxon
#'
#' @export
#' @param x (character) A vector of common or scientific names. Or, a
#' [taxon_state()] object
#' @param wiki_site (character) Wiki site. One of species (default), pedia,
#' commons
#' @param wiki (character) language. Default: en
#' @param ask logical; should get_wiki be run in interactive mode?
#' If `TRUE` and more than one wiki is found for the species, the user is
#' asked for input. If `FALSE` NA is returned for multiple matches.
#' @param messages logical; should progress be printed?
#' @param rows numeric; Any number from 1 to infinity. If the default NA, all
#' rows are considered. Note that this function still only gives back a wiki
#' class object with one to many identifiers. See [get_wiki_()] to get back
#' all, or a subset, of the raw data that you are presented during the ask
#' process.
#' @param limit (integer) number of records to return
#' @param ... Ignored
#' @param check logical; Check if ID matches any existing on the DB, only
#' used in [as.wiki()]
#' @template getreturn
#'
#' @details For `wiki_site = "pedia" `, we use the english language site by
#' default. Set the `wiki` parameter for a different language site.
#'
#' @family taxonomic-ids
#' @seealso [classification()]
#'
#' @examples \dontrun{
#' get_wiki(x = "Quercus douglasii")
#' get_wiki(x = "Quercu")
#' get_wiki(x = "Quercu", "pedia")
#' get_wiki(x = "Quercu", "commons")
#'
#' # diff. wikis with wikipedia
#' get_wiki("Malus domestica", "pedia")
#' get_wiki("Malus domestica", "pedia", "fr")
#'
#' # as coercion
#' as.wiki("Malus_domestica")
#' as.wiki("Malus_domestica", wiki_site = "commons")
#' as.wiki("Malus_domestica", wiki_site = "pedia")
#' as.wiki("Malus_domestica", wiki_site = "pedia", wiki = "fr")
#' as.wiki("Malus_domestica", wiki_site = "pedia", wiki = "da")
#' }

get_wiki <- function(x, wiki_site = "species", wiki = "en", ask = TRUE,
                     messages = TRUE, limit = 100, rows = NA, ...) {

  assert(x, c("character", "taxon_state"))
  assert(ask, "logical")
  assert(wiki_site, "character")
  assert(wiki, "character")
  assert(messages, "logical")
  assert_rows(rows)

  if (inherits(x, "character")) {
    tstate <- taxon_state$new(class = "wiki", names = x)
    items <- x
  } else {
    tstate <- x
    x <- tstate$taxa_remaining()
    items <- c(x, tstate$taxa_completed())
  }

  prog <- progressor$new(items = items, suppress = !messages)
  done <- tstate$get()
  for (i in seq_along(done)) prog$completed(names(done)[i], done[[i]]$att)
  prog$prog_start()

  for (i in seq_along(x)) {
    direct <- FALSE
    mssg(messages, "\nRetrieving data for taxon '", x, "'\n")
    df <- switch(
      wiki_site,
      species = wikitaxa::wt_wikispecies_search(query = x[i], limit = limit, ...),
      pedia = wikitaxa::wt_wikipedia_search(query = x[i], wiki = wiki,
                                            limit = limit, ...),
      commons = wikitaxa::wt_wikicommons_search(query = x[i], limit = limit, ...)
    )$query$search
    mm <- NROW(df) > 1

    if (!inherits(df, "tbl_df") || NROW(df) == 0) {
      id <- NA_character_
      att <- "not found"
    } else {
      df <- df[, c("title", "size", "wordcount")]
      df <- sub_rows(df, rows)

      # should return NA if spec not found
      if (NROW(df) == 0) {
        mssg(messages, tx_msg_not_found)
        id <- NA_character_
        att <- "not found"
      }

      df$title <- gsub("\\s", "_", df$title)

      # take the one wiki from data.frame
      if (NROW(df) == 1) {
        id <- df$title
        att <- "found"
      }

      # check for direct match
      if (NROW(df) > 1) {
        df <- data.frame(df, stringsAsFactors = FALSE)
        matchtmp <- df[tolower(df$title) %in% tolower(x[i]), "title"]
        if (length(matchtmp) == 1) {
          id <- matchtmp
          direct <- TRUE
          att <- "found"
        } else {
          direct <- FALSE
          id <- NA_character_
          att <- m_na_ask_false_no_direct
          warning("> 1 result; no direct match found", call. = FALSE)
        }
      }

      # multiple matches
      if (any(
        NROW(df) > 1 && is.na(id) |
        NROW(df) > 1 && att == "found" && length(id) > 1
      )) {
        if (ask) {
          # user prompt
          df <- df[order(df$title), ]
          rownames(df) <- NULL

          # prompt
          message("\n\n")
          print(df)
          message("\nMore than one wiki ID found for taxon '", x[i], "'!\n
                  Enter rownumber of taxon (other inputs will return 'NA'):\n")
          take <- scan(n = 1, quiet = TRUE, what = 'raw')

          if (length(take) == 0) {
            take <- 'notake'
            att <- 'nothing chosen'
          }
          if (take %in% seq_len(nrow(df))) {
            take <- as.numeric(take)
            message("Input accepted, took taxon '",
                    as.character(df$title[take]), "'.\n")
            id <-  df$title[take]
            att <- "found"
          } else {
            id <- NA_character_
            mssg(messages, "\nReturned 'NA'!\n\n")
            att <- "not found"
          }
        } else {
          if (length(id) != 1) {
            warning(sprintf(m_more_than_one_found, "Wiki ID", x),
              call. = FALSE)
            id <- NA_character_
            att <- m_na_ask_false
          }
        }
      }

    }

    res <- list(id = as.character(id), att = att, multiple = mm,
      direct = direct)
    prog$completed(x[i], att)
    prog$prog(att)
    tstate$add(x[i], res)
  }
  out <- tstate$get()
  ids <- structure(pluck_un(out, "id", ""), class = "wiki",
    match = pluck_un(out, "att", ""),
    multiple_matches = pluck_un(out, "multiple", logical(1)),
    pattern_match = pluck_un(out, "direct", logical(1))
  )
  on.exit(prog$prog_summary(), add = TRUE)
  on.exit(tstate$exit, add = TRUE)
  attr(ids, 'wiki_site') <- wiki_site
  attr(ids, 'wiki_lang') <- wiki
  if ( !all(is.na(ids)) ) {
    zz <- gsub("\\s", "_", na.omit(ids))
    base_url <- switch(
      wiki_site,
      species = 'https://species.wikimedia.org/wiki/',
      pedia = sprintf('https://%s.wikipedia.org/wiki/', wiki),
      commons = 'https://commons.wikimedia.org/wiki/'
    )
    attr(ids, 'uri') <- paste0(base_url, zz)
  }
  return(ids)
}

#' @export
#' @rdname get_wiki
as.wiki <- function(x, check=TRUE, wiki_site = "species", wiki = "en") {
  UseMethod("as.wiki")
}

#' @export
#' @rdname get_wiki
as.wiki.wiki <- function(x, check=TRUE, wiki_site = "species",
                         wiki = "en") x

#' @export
#' @rdname get_wiki
as.wiki.character <- function(x, check=TRUE, wiki_site = "species",
                              wiki = "en") {
  if (length(x) == 1) {
    make_wiki(x, check, wiki_site, wiki)
  } else {
    collapse(x, make_wiki, "wiki", check = check)
  }
}

#' @export
#' @rdname get_wiki
as.wiki.list <- function(x, check=TRUE, wiki_site = "species",
                         wiki = "en") {
  if (length(x) == 1) {
    make_wiki(x, check)
  } else {
    collapse(x, make_wiki, "wiki", check = check)
  }
}

#' @export
#' @rdname get_wiki
as.wiki.numeric <- function(x, check=TRUE, wiki_site = "species",
                            wiki = "en") {
  as.wiki(as.character(x), check)
}

#' @export
#' @rdname get_wiki
as.wiki.data.frame <- function(x, check=TRUE, wiki_site = "species",
                               wiki = "en") {

  structure(x$ids, class = "wiki", match = x$match,
            multiple_matches = x$multiple_matches,
            pattern_match = x$pattern_match,
            wiki_site = x$wiki_site,
            wiki_lang = x$wiki_lang, uri = x$uri)
}

#' @export
#' @rdname get_wiki
as.data.frame.wiki <- function(x, ...){
  data.frame(ids = unclass(x),
             class = "wiki",
             match = attr(x, "match"),
             multiple_matches = attr(x, "multiple_matches"),
             pattern_match = attr(x, "pattern_match"),
             wiki_site = attr(x, 'wiki_site'),
             wiki_lang = attr(x, 'wiki_lang'),
             uri = attr(x, "uri"),
             stringsAsFactors = FALSE)
}

make_wiki <- function(x, check = TRUE, wiki_site, wiki) {
  url <- switch(
    wiki_site,
    species = 'https://species.wikimedia.org/wiki/%s',
    pedia = paste0(sprintf('https://%s.wikipedia.org/wiki', wiki), "/%s"),
    commons = 'https://commons.wikimedia.org/wiki/%s'
  )
  make_wiki_generic(x, url, "wiki", check)
}

check_wiki <- function(x) {
  tt <- wikitaxa::wt_wiki_page(x)
  identical(tt$status_code, 200)
}

#' @export
#' @rdname get_wiki
get_wiki_ <- function(x, messages = TRUE, wiki_site = "species",
                      wiki = "en", limit = 100, rows = NA, ...) {
  stats::setNames(
    lapply(x, get_wiki_help, messages = messages, wiki_site = wiki_site,
           wiki = wiki, limit = limit, rows = rows, ...),
    x
  )
}

get_wiki_help <- function(x, messages, wiki_site = "species", wiki = "en",
                          limit = 100, rows, ...) {

  mssg(messages, "\nRetrieving data for taxon '", x, "'\n")
  assert(x, "character")
  assert(wiki_site, "character")
  assert(wiki, "character")

  df <- switch(
    wiki_site,
    species = wikitaxa::wt_wikispecies_search(query = x, limit = limit, ...),
    pedia = wikitaxa::wt_wikipedia_search(query = x, wiki = wiki,
                                          limit = limit, ...),
    commons = wikitaxa::wt_wikicommons_search(query = x, limit = limit, ...)
  )$query$search

  if (!inherits(df, "tbl_df") || NROW(df) == 0) {
    NULL
  } else {
    df <- df[, c("title", "size", "wordcount")]
    sub_rows(df, rows)
  }
}
