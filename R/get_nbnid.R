#' Get the UK National Biodiversity Network ID from taxonomic names.
#'
#' @export
#' @param sci_com character; a vector of common or scientific names. Or, a
#' `taxon_state` object (see [taxon-state])
#' @param ask logical; should get_nbn be run in interactive mode?
#' If TRUE and more than one ID is found for the species, the user is asked for
#' input. If FALSE NA is returned for multiple matches.
#' @param messages logical; If TRUE the actual taxon queried is printed on the
#' console.
#' @param rec_only (logical) If `TRUE` ids of recommended names are
#' returned (i.e. synonyms are removed). Defaults to `FALSE`. Remember,
#' the id of a synonym is a taxa with 'recommended' name status.
#' @param rank (character) If given, we attempt to limit the results to those
#' taxa with the matching rank.
#' @param rows numeric; Any number from 1 to infinity. If the default NA, all
#' rows are considered. Note that this function still only gives back a nbn
#' class object with one to many identifiers. See
#' [get_nbn_()] to get back all, or a subset, of the raw
#' data that you are presented during the ask process.
#' @param name Deprecated, see `sci_com`
#' @param ... Further args passed on to `nbn_search`
#' @param x Input to [as_nbn()]
#' @param check logical; Check if ID matches any existing on the DB, only
#' used in [as_nbn()]
#' @template getreturn
#' @references https://api.nbnatlas.org/
#' @return an object of class nbnid, a light wrapper around a character
#' string that is the taxonomic ID - includes attributes with relavant
#' metadata
#' @family taxonomic-ids
#' @family nbn
#' @seealso [classification()]
#'
#' @author Scott Chamberlain, 
#'
#' @examples \dontrun{
#' get_nbn(sci_com='Poa annua')
#' get_nbn(sci_com='Poa annua', rec_only=TRUE)
#' get_nbn(sci_com='Poa annua', rank='Species')
#' get_nbn(sci_com='Poa annua', rec_only=TRUE, rank='Species')
#' get_nbn(sci_com='Pinus contorta')
#'
#' # The NBN service handles common names too
#' get_nbn(sci_com='red-winged blackbird')
#'
#' # specify rows to limit choices available
#' get_nbn('Poa ann')
#' get_nbn('Poa ann', rows=1)
#' get_nbn('Poa ann', rows=25)
#' get_nbn('Poa ann', rows=1:2)
#'
#' # When not found
#' get_nbn(sci_com="uaudnadndj")
#' get_nbn(c("Zootoca vivipara", "uaudnadndj"))
#' get_nbn(c("Zootoca vivipara","Chironomus riparius", "uaudnadndj"))
#'
#' # Convert an nbn without class information to a nbn class
#' as_nbn(get_nbn("Zootoca vivipara")) # already a nbn, returns the same
#' as_nbn(get_nbn(c("Zootoca vivipara","Pinus contorta"))) # same
#' as_nbn('NHMSYS0001706186') # character
#' # character vector, length > 1
#' as_nbn(c("NHMSYS0001706186","NHMSYS0000494848","NBNSYS0000010867"))
#' # list
#' as_nbn(list("NHMSYS0001706186","NHMSYS0000494848","NBNSYS0000010867"))
#' ## dont check, much faster
#' as_nbn('NHMSYS0001706186', check=FALSE)
#' as_nbn(list("NHMSYS0001706186","NHMSYS0000494848","NBNSYS0000010867"),
#'   check=FALSE)
#'
#' (out <- as_nbn(c("NHMSYS0001706186","NHMSYS0000494848",
#'   "NBNSYS0000010867")))
#' data.frame(out)
#' as_nbn( data.frame(out) )
#'
#' # Get all data back
#' get_nbn_("Zootoca vivipara")
#' get_nbn_("Poa annua", rows=2)
#' get_nbn_("Poa annua", rows=1:2)
#' get_nbn_(c("asdfadfasd","Pinus contorta"), rows=1:5)
#'
#' # use curl options
#' invisible(get_nbn("Quercus douglasii", verbose = TRUE))
#' }

get_nbn <- function(sci_com, ask = TRUE, messages = TRUE, rec_only = FALSE,
  rank = NULL, rows = NA, name = NULL, ...) {

  assert(sci_com, c("character", "taxon_state"))
  assert(ask, "logical")
  assert(rec_only, "logical")
  assert(rank, "character")
  assert(messages, "logical")
  assert_rows(rows)
  pchk(name, "sci_com")

  if (inherits(sci_com, "character")) {
    tstate <- taxon_state$new(class = "nbn", names = sci_com)
    items <- sci_com
  } else {
    assert_state(sci_com, "nbn")
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
    rank_taken <- NA_character_
    id <- NA_character_
    mssg(messages, "\nRetrieving data for taxon '", sci_com[i], "'\n")
    df <- nbn_search(sci_com = sci_com[i], rows = 500, fq = "idxtype:TAXON",
      ...)$data
    if (is.null(df) || length(df) == 0) df <- data.frame(NULL)
    mm <- NROW(df) > 1

    if (NROW(df) == 0) {
      mssg(messages, m_not_found_sp_altclass)
      att <- 'not found'
    } else {
      if (rec_only) df <- df[ df$taxonomicStatus == 'accepted', ]
      if (!is.null(rank)) df <- df[ tolower(df$rank) == tolower(rank), ]
      df <- sub_rows(df, rows)
      df <- df[,c('guid', 'scientificName', 'rank', 'taxonomicStatus')]
      names(df)[1] <- 'nbnid'
      id <- df$nbnid
      rank_taken <- as.character(df$rank)
      att <- 'found'
    }

    # not found on NBN
    if (length(id) == 0) {
      mssg(messages, m_not_found_sp_altclass)
      id <- NA_character_
      att <- 'not found'
    }
    # more than one, try for direct match
    if (length(id) > 1) {
      matchtmp <- df[tolower(df$scientificName) %in% tolower(sci_com[i]), ]
      if (NROW(matchtmp) == 1) {
        id <- matchtmp$nbnid
        rank_taken <- as.character(matchtmp$rank)
        name <- matchtmp$scientificName
        direct <- TRUE
        att <- "found"
      }
    }

    # more than one found -> user input
    if (length(id) > 1) {
      if (ask) {
        rownames(df) <- seq_len(NROW(df))
        # prompt
        message("\n\n")
        message("\nMore than one NBN ID found for taxon '", sci_com[i], "'!\n
            Enter rownumber of taxon (other inputs will return 'NA'):\n")
        print(df)
        take <- scan(n = 1, quiet = TRUE, what = 'raw')

        if (length(take) == 0) {
          take <- 'notake'
          att <- 'nothing chosen'
        }
        if (take %in% seq_len(nrow(df))) {
          take <- as.numeric(take)
          message("Input accepted, took NBN ID '",
                  as.character(df$nbnid[take]), "'.\n")
          id <- as.character(df$nbnid[take])
          rank_taken <- as.character(df$rank[take])
          name <- df$scientificName[take]
          att <- 'found'
        } else {
          id <- NA_character_
          att <- 'not found'
          mssg(messages, "\nReturned 'NA'!\n\n")
        }
      } else{
        if (length(id) != 1) {
          warning(sprintf(m_more_than_one_found, "NBN ID", sci_com[i]),
            call. = FALSE)
          id <- NA_character_
          att <- m_na_ask_false
        }
      }
    }
    res <- list(id = id, name = name, rank = rank_taken, att = att,
      multiple = mm, direct = direct)
    prog$completed(sci_com[i], att)
    prog$prog(att)
    tstate$add(sci_com[i], res)
  }
  out <- tstate$get()
  res <- make_taxa_taxon(out, "nbn")
  on.exit(prog$prog_summary(), add = TRUE)
  on.exit(tstate$exit, add = TRUE)
  return(res)
}
#' @export
#' @rdname get_nbn
get_nbnid <- function(...) {
  fchk("get_nbnid", "get_nbn")
  get_nbn(...)
}

#' @export
#' @rdname get_nbn
as_nbn <- function(x, check=TRUE) UseMethod("as_nbn")

#' @export
#' @rdname get_nbn
as_nbn.nbn <- function(x, check=TRUE) x

#' @export
#' @rdname get_nbn
as_nbn.character <- function(x, check=TRUE) {
  if (length(x) == 1) {
    make_nbn(x, check)
  } else {
    collapse(x, make_nbn, "nbn", check = check)
  }
}

#' @export
#' @rdname get_nbn
as_nbn.list <- function(x, check=TRUE) {
  if (length(x) == 1) {
    make_nbn(x, check)
  } else {
    collapse(x, make_nbn, "nbn", check = check)
  }
}

#' @export
#' @rdname get_nbn
as_nbn.data.frame <- function(x, check=TRUE) as_txid_df(x, check)

make_nbn <- function(x, check=TRUE) {
  make_generic(x, 'https://species-ws.nbnatlas.org/species/%s', "nbn", check)
}

check_nbn <- function(x){
  url <- "https://species-ws.nbnatlas.org/species/"
  res <- tax_GET_nocheck(paste0(url, x))
  if (res$status_code == 200) TRUE else FALSE
}

#' @export
#' @rdname get_nbn
get_nbn_ <- function(sci_com, messages = TRUE, rec_only = FALSE, rank = NULL,
                       rows = NA, name = NULL, ...) {
  pchk(name, "sci_com")
  stats::setNames(lapply(sci_com, get_nbn_help, messages = messages,
                  rec_only = rec_only, rank = rank, rows = rows, ...), sci_com)
}
#' @export
#' @rdname get_nbn
get_nbnid_ <- function(...) {
  fchk("get_nbnid_", "get_nbn_")
  get_nbn_(...)
}

get_nbn_help <- function(name, messages, rec_only, rank, rows, ...){
  mssg(messages, "\nRetrieving data for taxon '", name, "'\n")
  df <- nbn_search(sci_com = name, all = TRUE, ...)$data
  if (is.null(df)) df <- data.frame(NULL)

  if (NROW(df) == 0) {
    NULL
  } else {
    if (rec_only) df <- df[ df$taxonomicStatus == 'accepted', ]
    if (!is.null(rank)) df <- df[ df$rank == rank, ]
    df <- df[,c('guid', 'scientificName', 'rank', 'taxonomicStatus')]
    if (NROW(df) == 0) NULL else sub_rows(df, rows)
  }
}
