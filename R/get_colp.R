#' get_colp
#' 
#' Get ids from Catalogue of Life Plus for given taxonomic names 
#'
#' @export
#' @param sci character; one or more scientific names. Or, a `taxon_state`
#' object (see [taxon-state])
#' @param ask logical; should get_colp be run in interactive mode? If TRUE and
#' more than one id is found for the species, the user is asked for input. If
#' FALSE NA is returned for multiple matches.
#' @param messages logical; If `TRUE` (default) the actual taxon queried is
#' printed on the console.
#' @param rows numeric; Any number from 1 to infinity. If the default NA, all
#' rows are considered. Note that this function still only gives back a id
#' class object with one to many identifiers. See [get_colp_()] to get back
#' all, or a subset, of the raw data that you are presented during the ask
#' process.
#' @param x Input to [as.colp()]
#' @param ... Ignored
#' @param check logical; Check if ID matches any existing on the DB, only used
#' in [as.colp()]
#' @template getreturn
#' @family taxonomic-ids
#' @seealso [classification()]
#' @note CURRENTLY USING DATASETKEY=3 IN THE TAXON NAME SEARCH
#' @examples \dontrun{
#' get_colp(sci="Chironomus riparius")
#' get_colp(sci=c("Puma concolor", "Chironomus riparius", "Abies", "Felidae"))
#' get_colp(sci=c("Chironomus riparius", "aaa vva"))
#'
#' # When not found
#' get_colp(sci="howdy")
#' get_colp(sci=c("Chironomus riparius", "howdy"))
#'
#' # Convert a id without class information to a ncbi class
#' as.colp(get_colp("Chironomus riparius")) # already ncbi class, returns same
#'
#' # Get all data back
#' get_colp_("Puma concolor")
#' }
get_colp <- function(sci, ask = TRUE, messages = TRUE, rows = NA, ...) {
  assert(sci, c("character", "taxon_state"))
  assert(ask, "logical")
  assert(messages, "logical")
  assert_rows(rows)

  if (inherits(sci, "character")) {
    tstate <- taxon_state$new(class = "colp", names = sci)
    items <- sci
  } else {
    assert_state(sci, "colp")
    tstate <- sci
    sci <- tstate$taxa_remaining()
    items <- c(sci, tstate$taxa_completed())
  }

  prog <- progressor$new(items = items, suppress = !messages)
  done <- tstate$get()
  for (i in seq_along(done)) prog$completed(names(done)[i], done[[i]]$att)
  prog$prog_start()

  for (i in seq_along(sci)) {
    direct <- FALSE
    att <- rank_taken <- name <- NA_character_
    mssg(messages, "\nRetrieving data for taxon '", sci[i], "'\n")
    res <- colpluz::cp_nu_search(sci[i], dataset_key=3, limit=500)
    df <- tibble::tibble()
    if ('usage' %in% names(res$result))
      df <- tibble::as_tibble(res$result$usage$name)
    mm <- NROW(df) > 1

    if (NROW(df) == 0) {
      mssg(messages, m_not_found_sp_altclass)
      id <- NA_character_
      att <- 'NA due to not found'
    } else {
      if (NROW(df) == 1) {
        direct <- TRUE
        att <- "found"
        rank_taken <- df$rank
        name <- df$scientificName
      }

      # more than one found on colp -> user input
      if (NROW(df) > 1) {
        df <- sub_rows(df, rows)
        id <- df$id
        
        if (length(id) == 0) {
          id <- NA_character_
        }

        if (length(id) == 1) {
          direct <- TRUE
          att <- "found"
          rank_taken <- df$rank
          name <- df$scientificName
        }

        if (length(id) > 1) {
          # check for exact match
          matchtmp <- df[
            tolower(
              as.character(df$scientificName)) %in% tolower(sci[i]), ]
          if (NROW(matchtmp) == 1) {
            id <- as.character(matchtmp$id)
            rank_taken <- matchtmp$rank
            name <- matchtmp$scientificName
            direct <- TRUE
          }
        }

        if (length(id) > 1) {
          if (!ask) {
            if (length(id) == 1) {
              att <- "found"
              rank_taken <- df$rank
              name <- df$scientificName
            } else {
              warning(
                sprintf(m_more_than_one_found, "id", sci[i]),
                call. = FALSE
              )
              id <- NA_character_
              att <- m_na_ask_false
            }
          } else {
            # prompt
            df <- tibble::remove_rownames(df)
            message("\n\n")
            message("\nMore than one id found for taxon '", sci[i], "'!\n
              Enter rownumber of taxon (other inputs will return 'NA'):\n")
            print(df)
            take <- scan(n = 1, quiet = TRUE, what = 'raw')

            if (length(take) == 0) {
              take <- "notake"
              att <- "nothing chosen"
            }
            if (take %in% seq_len(nrow(df))) {
              take <- as.numeric(take)
              message("Input accepted, took id '",
                      as.character(df$id[take]), "'.\n")
              id <- as.character(df$id[take])
              rank_taken <- df$rank[take]
              name <- df$scientificName[take]
              att <- 'found'
            } else {
              id <- NA_character_
              att <- "NA due to user input out of range"
              mssg(messages, "\nReturned 'NA'!\n\n")
            }
          }
        }
      }
    }
    res <- list(id = as.character(id), name = name, rank = rank_taken,
      att = att, multiple = mm, direct = direct)
    prog$completed(sci[i], att)
    prog$prog(att)
    tstate$add(sci[i], res)
  }
  out <- tstate$get()
  ids <- as.character(unlist(pluck_un(out, "id", "")))
  res <- taxa_taxon(
    name = unlist(pluck(out, "name")) %||% NA_character_,
    id = taxa::taxon_id(ids, db = "colp"),
    rank = unlist(pluck(out, "rank")) %||% NA_character_,
    uri = NA_character_,
    match = unname(unlist(pluck(out, "att"))),
    multiple_matches = unname(unlist(pluck(out, "multiple"))),
    pattern_match = unname(unlist(pluck(out, "direct"))),
    class = "colp"
  )
  on.exit(prog$prog_summary(), add = TRUE)
  on.exit(tstate$exit, add = TRUE)
  return(res)
}

#' @export
#' @rdname get_colp
as.colp <- function(x, check=TRUE) UseMethod("as.colp")

#' @export
#' @rdname get_colp
as.colp.colp <- function(x, check=TRUE) x

#' @export
#' @rdname get_colp
as.colp.character <- function(x, check=TRUE) if(length(x) == 1) make_colp(x, check) else collapse(x, make_colp, "colp", check=check)

#' @export
#' @rdname get_colp
as.colp.list <- function(x, check=TRUE) if(length(x) == 1) make_colp(x, check) else collapse(x, make_colp, "colp", check=check)

#' @export
#' @rdname get_colp
as.colp.numeric <- function(x, check=TRUE) as.colp(as.character(x), check)

#' @export
#' @rdname get_colp
as.colp.data.frame <- function(x, check=TRUE) as_txid_df(x, check)

make_colp <- function(x, check=TRUE) {
  make_generic(x, 'https://www.ncbi.nlm.nih.gov/taxonomy/%s',
    "colp", check)
}

check_colp <- function(x){
  key <- getkey(NULL, "ENTREZ_KEY")
  cli <- crul::HttpClient$new(url = colp_base(), headers = tx_ual,
    opts = list(http_version = 2L))
  args <- tc(list(db = "taxonomy", id = x, api_key = key))
  res <- cli$get("entrez/eutils/esummary.fcgi", query = args)
  res$raise_for_status()
  tt <- xml2::read_xml(res$parse("UTF-8"))
  tryid <- xml2::xml_text(xml2::xml_find_all(tt, "//Id"))
  identical(as.character(x), tryid)
}


#' @export
#' @rdname get_colp
get_colp_ <- function(sci, messages = TRUE, rows = NA, ...) {
  xxx
}

colp_base <- function() "https://api.catalogue.life/"
