#' Get the EOL ID from Encyclopedia of Life from taxonomic names.
#'
#' Note that EOL doesn't expose an API endpoint for directly querying for EOL
#' taxon ID's, so we first use the function [eol_search()]
#' to find pages that deal with the species of interest, then use
#' [eol_pages()] to find the actual taxon IDs.
#'
#' @export
#' @param sci_com character; one or more scientific or common names. Or,
#' a `taxon_state` object (see [taxon-state])
#' @param ask logical; should get_eolid be run in interactive mode?
#' If TRUE and more than one ID is found for the species, the user is asked for
#' input. If FALSE NA is returned for multiple matches.
#' @param ... Further args passed on to [eol_search()]
#' @param messages logical; If `TRUE` the actual taxon queried is printed
#' on the console.
#' @param rows numeric; Any number from 1 to infinity. If the default NA, all
#' rows are considered. Note that this function still only gives back a eolid
#' class object with one to many identifiers. See
#' [get_eolid_()] to get back all, or a subset, of the raw
#' data that you are presented during the ask process.
#' @param rank (character) A taxonomic rank name. See [rank_ref()]
#' for possible options. Though note that some data sources use atypical ranks,
#' so inspect the data itself for options. Optional. See `Filtering` below.
#' @param data_source (character) A data source inside of EOL. These are
#' longish names like e.g., "Barcode of Life Data Systems" or
#' "USDA PLANTS images". Optional. See `Filtering` below.
#' @param x Input to [as.eolid()]
#' @param check logical; Check if ID matches any existing on the DB, only
#' used in [as.eolid()]
#' @param sciname Deprecated, see `sci_com`
#' @template getreturn
#'
#' @family taxonomic-ids
#' @seealso [classification()]
#'
#' @author Scott Chamberlain, \email{myrmecocystus@@gmail.com}
#'
#' @details EOL is a bit odd in that they have page IDs for each taxon, but
#' then within that, they have taxon ids for various taxa within that page
#' (e.g., GBIF and NCBI each have a taxon they refer to within the
#' page \[i.e., taxon\]). And we need the taxon ids from a particular data
#' provider (e.g, NCBI) to do other things, like get a higher classification
#' tree. However, humans want the page id, not the taxon id. So, the
#' id returned from this function is the taxon id, not the page id. You can
#' get the page id for a taxon by using [eol_search()] and
#' `[eol_pages()], and the URI returned in the attributes for a
#' taxon will lead you to the taxon page, and the ID in the URL is the
#' page id.
#'
#' @section Filtering:
#' The parameters `rank` and `data_source` are not used in the
#' search to the data provider, but are used in filtering the data down to a
#' subset that is closer to the target you want.  For all these parameters,
#' you can use regex strings since we use [grep()] internally to
#' match. Filtering narrows down to the set that matches your query, and
#' removes the rest.
#'
#' @examples \dontrun{
#' get_eolid(sci_com='Pinus contorta')
#' get_eolid(sci_com='Puma concolor')
#'
#' get_eolid(c("Puma concolor", "Pinus contorta"))
#'
#' # specify rows to limit choices available
#' get_eolid('Poa annua')
#' get_eolid('Poa annua', rows=1)
#' get_eolid('Poa annua', rows=2)
#' get_eolid('Poa annua', rows=1:2)
#'
#' # When not found
#' get_eolid(sci_com="uaudnadndj")
#' get_eolid(c("Chironomus riparius", "uaudnadndj"))
#'
#' # filter results to a rank or data source, or both
#' get_eolid("Satyrium")
#' get_eolid("Satyrium", rank = "genus")
#' get_eolid("Satyrium", data_source = "INAT")
#' get_eolid("Satyrium", rank = "genus",
#'   data_source = "North Pacific Species List")
#'
#' # Convert a eolid without class information to a eolid class
#' # already a eolid, returns the same
#' as.eolid(get_eolid("Chironomus riparius"))
#' # same
#' as.eolid(get_eolid(c("Chironomus riparius","Pinus contorta")))
#' # numeric
#' as.eolid(10247706)
#' # numeric vector, length > 1
#' as.eolid(c(6985636,12188704,10247706))
#' # character
#' as.eolid("6985636")
#' # character vector, length > 1
#' as.eolid(c("6985636","12188704","10247706"))
#' # list, either numeric or character
#' as.eolid(list("6985636","12188704","10247706"))
#' ## dont check, much faster
#' as.eolid("6985636", check=FALSE)
#' as.eolid(6985636, check=FALSE)
#' as.eolid(c("6985636","12188704","10247706"), check=FALSE)
#' as.eolid(list("6985636","12188704","10247706"), check=FALSE)
#'
#' (out <- as.eolid(c(6985636,12188704,10247706)))
#' data.frame(out)
#' as.eolid( data.frame(out) )
#'
#' # Get all data back
#' get_eolid_("Poa annua")
#' get_eolid_("Poa annua", rows=2)
#' get_eolid_("Poa annua", rows=1:2)
#' get_eolid_(c("asdfadfasd", "Pinus contorta"))
#' }

get_eolid <- function(sci_com, ask = TRUE, messages = TRUE,
  rows = NA, rank = NULL, data_source = NULL, sciname = NULL, ...) {

  assert(sci_com, c("character", "taxon_state"))
  assert(ask, "logical")
  assert(messages, "logical")
  assert(rank, "character")
  assert(data_source, "character")
  assert_rows(rows)
  pchk(sciname, "sci_com")

  if (inherits(sci_com, "character")) {
    tstate <- taxon_state$new(class = "eolid", names = sci_com)
    items <- sci_com
  } else {
    assert_state(sci_com, "eolid")
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
    tmp <- eol_search(sci = sci_com[i], ...)
    datasource <- NA_character_
    if (all(is.na(tmp))) {
      mssg(messages, m_not_found_sp_altclass)
      id <- NA_character_
      page_id <- NA_character_
      att <- "not found"
      mm <- FALSE
    } else {
      pageids <- tmp[grep(tolower(sci_com[i]), tolower(tmp$name)), "pageid"]

      if (length(pageids) == 0) {
        if (nrow(tmp) > 0)
        mssg(messages, paste(m_not_found_sp_altclass, sprintf("\nDid find: %s",
                                        paste(tmp$name, collapse = "; "))))
        id <- NA_character_
      } else {
        dfs <- lapply(pageids, function(x) {
          y <- tryCatch(eol_pages(x), error = function(e) e)
          if (is(y, "error")) NULL else y$scinames
        })
        names(dfs) <- pageids
        dfs <- tc(dfs)
        if (length(dfs) > 1) dfs <- dfs[!sapply(dfs, nrow) == 0]
        df <- dt2df(dfs)
        df <- rename(df, c(".id" = "pageid", "identifier" = "eolid",
          "scientificname" = "name", "nameaccordingto" = "source",
          "taxonrank" = "rank"))

        # drop columns
        df$canonicalform <- df$sourceidentifier <- NULL
        mm <- NROW(df) > 1
        df <- sub_rows(df, rows)

        if (nrow(df) == 0) {
          mssg(messages, m_not_found_sp_altclass)
          id <- NA_character_
          page_id <- NA_character_
        } else{
          id <- df$eolid
        }
        names(id) <- df$pageid
      }
    }

    # not found on eol
    if (length(id) == 0 || all(is.na(id))) {
      mssg(messages, m_not_found_sp_altclass)
      id <- NA_character_
      page_id <- NA_character_
			mm <- FALSE
      att <- "not found"
    }
    # only one found on eol
    if (length(id) == 1 & !all(is.na(id))) {
      id <- df$eolid
      page_id <- df$pageid
      datasource <- df$source
      direct <- TRUE
      att <- "found"
    }

    # filter by rank, if given
    if (!is.null(rank) || !is.null(data_source)) {
      df <- filt(df, "rank", rank)
      df <- filt(df, "source", data_source)
      id <- df$eolid
      page_id <- df$pageid
      datasource <- df$source
      att <- "found"
    }

    if (length(id) == 0 || all(is.na(id))) {
      mssg(messages, m_not_found_sp_altclass)
      id <- NA_character_
      page_id <- NA_character_
      datasource <- NA_character_
      mm <- FALSE
      att <- "not found"
    }

    if (length(id) > 1) {
      matchtmp <- df[tolower(df$name) %in% tolower(sci_com[i]), ]
      if (NROW(matchtmp) == 1) {
        id <- matchtmp$eolid
        direct <- TRUE
        page_id <- matchtmp$pageid
        datasource <- matchtmp$source
        att <- "found"
      }
    }

    # more than one found on eol -> user input
    if (length(id) > 1) {
      if (ask) {
        rownames(df) <- 1:nrow(df)
        # prompt
        message("\n\n")
        message("\nMore than one eolid found for taxon '", sci_com[i], "'!\n
            Enter rownumber of taxon (other inputs will return 'NA'):\n")
        print(df)
        take <- scan(n = 1, quiet = TRUE, what = "raw")

        if (length(take) == 0) {
          take <- "notake"
        }
        if (take %in% seq_len(nrow(df))) {
          take <- as.numeric(take)
          message("Input accepted, took eolid '",
                  as.character(df$eolid[take]), "'.\n")
          id <- as.character(df$eolid[take])
          names(id) <- as.character(df$pageid[take])
          page_id <- as.character(df$pageid[take])
          datasource <- as.character(df$source[take])
          att <- "found"
        } else {
          id <- NA_character_
          page_id <- NA_character_
          att <- "not found"
          mssg(messages, "\nReturned 'NA'!\n\n")
        }
      } else {
        if (length(id) != 1) {
          warning(sprintf(m_more_than_one_found, "eolid", sci_com[i]),
            call. = FALSE)
          id <- NA_character_
          page_id <- NA_character_
          att <- m_na_ask_false
        }
      }
    }
    res <- list(id = as.character(id), page_id = page_id, source = datasource, 
      att = att, multiple = mm, direct = direct)
    prog$completed(sci_com[i], att)
    prog$prog(att)
    tstate$add(sci_com[i], res)
  }
  out <- tstate$get()
  page_ids <- pluck_un(out, "page_id", "")
  ids <- structure(
    pluck_un(out, "id", ""), class = "eolid",
    pageid = page_ids,
    provider = pluck_un(out, "source", ""),
    match = pluck_un(out, "att", ""),
    multiple_matches = pluck_un(out, "multiple", logical(1)),
    pattern_match = pluck_un(out, "direct", logical(1))
  )
  on.exit(prog$prog_summary(), add = TRUE)
  on.exit(tstate$exit, add = TRUE)
  add_uri(ids, get_url_templates$eol, page_ids)
}

taxize_sort_df <- function(data, vars = names(data)) {
  if (length(vars) == 0 || is.null(vars))
    return(data)
  data[do.call("order", data[, vars, drop = FALSE]), , drop = FALSE]
}

#' @export
#' @rdname get_eolid
as.eolid <- function(x, check=TRUE) {
  UseMethod("as.eolid")
}

#' @export
#' @rdname get_eolid
as.eolid.eolid <- function(x, check=TRUE) {
  x
}

#' @export
#' @rdname get_eolid
as.eolid.character <- function(x, check=TRUE) {
  if (length(x) == 1) {
    make_eolid(x, check)
  } else {
    collapse(x, fxn = make_eolid, class = "eolid", check = check)
  }
}

#' @export
#' @rdname get_eolid
as.eolid.list <- function(x, check=TRUE) {
  if (length(x) == 1) {
    make_eolid(x, check)
  } else {
    collapse(x, make_eolid, "eolid", check = check)
  }
}

#' @export
#' @rdname get_eolid
as.eolid.numeric <- function(x, check=TRUE) {
  as.eolid(as.character(x), check)
}

#' @export
#' @rdname get_eolid
as.eolid.data.frame <- function(x, check=TRUE) {
  structure(x$ids, class = "eolid", match = x$match,
            multiple_matches = x$multiple_matches,
            pattern_match = x$pattern_match, uri = x$uri)
}

#' @export
#' @rdname get_eolid
as.data.frame.eolid <- function(x, ...){
  data.frame(ids = as.character(unclass(x)),
             class = "eolid",
             match = attr(x, "match"),
             multiple_matches = attr(x, "multiple_matches"),
             pattern_match = attr(x, "pattern_match"),
             uri = attr(x, "uri"),
             stringsAsFactors = FALSE)
}

make_eolid <- function(x, check=TRUE) {
  tmp <- make_generic(x, 'https://eol.org/pages/%s/overview', "eolid", check)
  if (!check) {
    attr(tmp, "uri") <- NA_character_
  } else {
    z <- get_eol_pageid(x)
    if (!is.na(z)) {
      attr(tmp, "uri") <- sprintf('https://eol.org/pages/%s/overview', z)
    } else {
      attr(tmp, "uri") <- NA_character_
    }
  }
  tmp
}

check_eolid <- function(x) {
  url <- sprintf("https://eol.org/api/hierarchy_entries/1.0/%s.json", x)
  tryid <- tax_GET_nocheck(url)
  if (tryid$status_code == 200) TRUE else FALSE
}

get_eol_pageid <- function(x) {
  url <- sprintf("https://eol.org/api/hierarchy_entries/1.0/%s.json", x)
  tt <- tax_GET_nocheck(url)
  if (tt$status_code == 200) {
    jsonlite::fromJSON(tt$parse("UTF-8"), FALSE)$taxonConceptID
  } else {
    NA
  }
}

#' @export
#' @rdname get_eolid
get_eolid_ <- function(sci_com, messages = TRUE, rows = NA, sciname = NULL,
  ...) {
  pchk(sciname, "sci_com")
  stats::setNames(lapply(sci_com, get_eolid_help, messages = messages,
                  rows = rows, ...), sci_com)
}

get_eolid_help <- function(sci_com, messages, rows, ...) {
  mssg(messages, "\nRetrieving data for taxon '", sci_com, "'\n")
  tmp <- eol_search(terms = sci_com, ...)

  if (all(is.na(tmp))) {
    NULL
  } else {
    pageids <- tmp[grep(tolower(sci_com), tolower(tmp$name)), "pageid"]
    if (length(pageids) == 0) {
      NULL
    } else {
      dfs <- lapply(pageids, function(x) {
        y <- tryCatch(eol_pages(x), error = function(e) e)
        if (inherits(y, "error")) NULL else y$scinames
      })
      names(dfs) <- pageids
      dfs <- tc(dfs)
      if (length(dfs) > 1) dfs <- dfs[!sapply(dfs, nrow) == 0]
      dfs <- dt2df(dfs)
      df <- dfs[,c('.id','identifier','scientificname','nameaccordingto')]
      names(df) <- c('pageid','eolid','name','source')
      if (NROW(df) == 0) NULL else sub_rows(df, rows)
    }
  }
}
