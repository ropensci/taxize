#' Get the BOLD (Barcode of Life) code for a search term.
#'
#' @importFrom bold bold_tax_name bold_tax_id
#' @export
#' @param sci character; A vector of scientific names. Or,
#' a `taxon_state` object (see [taxon-state])
#' @param fuzzy (logical) Whether to use fuzzy search or not (default: FALSE).
#' @param dataTypes (character) Specifies the datatypes that will be returned.
#' See [bold_search()] for options.
#' @param includeTree (logical) If TRUE (default: FALSE), returns a list
#' containing information for parent taxa as well as the specified taxon.
#' @param ask logical; should get_itis be run in interactive mode?
#' If TRUE and more than one TSN is found for teh species, the user is asked for
#' input. If FALSE NA is returned for multiple matches.
#' @param messages logical; should progress be printed?
#' @param x Input to [`as_bold()`]
#' @param ... Curl options passed on to [`crul::verb-GET`]
#' @param rows numeric; Any number from 1 to infinity. If the default NA, all rows are
#' considered. Note that this function still only gives back a bold class object with one
#' to many identifiers. See [get_bold_()] to get back all, or a subset,
#' of the raw data that you are presented during the ask process.
#' @param division (character) A division (aka phylum) name. Optional. See `Filtering`
#' below.
#' @param parent (character) A parent name (i.e., the parent of the target search
#' taxon). Optional. See `Filtering` below.
#' @param rank (character) A taxonomic rank name. See [rank_ref] for possible
#' options. Though note that some data sources use atypical ranks, so inspect the
#' data itself for options. Optional. See `Filtering` below.
#' @param check logical; Check if ID matches any existing on the DB, only used in
#' [as_bold()]
#' @param searchterm Deprecated, see `sci`
#' @template getreturn
#'
#' @section Filtering:
#' The parameters `division`, `parent`, and `rank` are not used in the search to
#' the data provider, but are used in filtering the data down to a subset that
#' is closer to the target you want.  For all these parameters, you can use
#' regex strings since we use [grep()] internally to match. Filtering narrows
#' down to the set that matches your query, and removes the rest.
#'
#' @family taxonomic-ids
#' @seealso [classification()]
#'
#' @examples \dontrun{
#' get_bold(sci = "Agapostemon")
#' get_bold(sci = "Chironomus riparius")
#' get_bold(c("Chironomus riparius","Quercus douglasii"))
#' splist <- names_list('species')
#' get_bold(splist, messages=FALSE)
#'
#' # Fuzzy searching
#' get_bold(sci="Osmi", fuzzy=TRUE)
#'
#' # Get back a subset
#' get_bold(sci="Osmi", fuzzy=TRUE, rows = 1)
#' get_bold(sci="Osmi", fuzzy=TRUE, rows = 1:10)
#' get_bold(sci=c("Osmi","Aga"), fuzzy=TRUE, rows = 1)
#' get_bold(sci=c("Osmi","Aga"), fuzzy=TRUE, rows = 1:3)
#'
#' # found
#' get_bold("Cordulegaster erronea")
#' get_bold('Arigomphus furcifer')
#'
#' # When not found
#' get_bold('Epicordulia princeps')
#' get_bold("howdy")
#' get_bold(c("Chironomus riparius", "howdy"))
#' get_bold("Nasiaeshna pentacantha")
#'
#' # Narrow down results to a division or rank, or both
#' ## Satyrium example
#' ### Results w/o narrowing
#' get_bold("Satyrium")
#' ### w/ phylum
#' get_bold("Satyrium", division = "Plantae")
#' get_bold("Satyrium", division = "Animalia")
#'
#' ## Rank example
#' get_bold("Osmia", fuzzy = TRUE)
#' get_bold("Osmia", fuzzy = TRUE, rank = "genus")
#'
#' # Fuzzy filter on any filtering fields
#' ## uses grep on the inside
#' get_bold("Satyrium", division = "anim")
#' get_bold("Aga", fuzzy = TRUE, parent = "*idae")
#'
#' # Convert a bold without class information to a bold class
#' as_bold(get_bold("Agapostemon")) # already a bold, returns the same
#' as_bold(get_bold(c("Agapostemon","Quercus douglasii"))) # same
#' as_bold(1973) # numeric
#' as_bold(c(1973,101009,98597)) # numeric vector, length > 1
#' as_bold("1973") # character
#' as_bold(c("1973","101009","98597")) # character vector, length > 1
#' as_bold(list("1973","101009","98597")) # list, either numeric or character
#' ## dont check, much faster
#' as_bold("1973", check=FALSE)
#' as_bold(1973, check=FALSE)
#' as_bold(c("1973","101009","98597"), check=FALSE)
#' as_bold(list("1973","101009","98597"), check=FALSE)
#'
#' (out <- as_bold(c(1973,101009,98597)))
#' data.frame(out)
#' as_bold( data.frame(out) )
#'
#' # Get all data back
#' get_bold_("Osmia", fuzzy=TRUE, rows=1:5)
#' get_bold_("Osmia", fuzzy=TRUE, rows=1)
#' get_bold_(c("Osmi","Aga"), fuzzy=TRUE, rows = 1:3)
#' }

get_bold <- function(sci, fuzzy = FALSE, dataTypes = 'basic',
                       includeTree = FALSE, ask = TRUE, messages = TRUE,
                       rows = NA, rank = NULL, division = NULL,
                       parent = NULL, searchterm = NULL, ...) {

  assert(sci, c("character", "taxon_state"))
  assert(ask, "logical")
  assert(messages, "logical")
  assert(fuzzy, "logical")
  assert(dataTypes, "character")
  assert(includeTree, "logical")
  assert(rank, "character")
  assert(division, "character")
  assert(parent, "character")
  assert_rows(rows)
  pchk(searchterm, "sci")

  if (inherits(sci, "character")) {
    tstate <- taxon_state$new(class = "bold", names = sci)
    items <- sci
  } else {
    assert_state(sci, "bold")
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
    bold_df <- bold_search(sci = sci[i], fuzzy = fuzzy,
                           dataTypes = dataTypes,
                           includeTree = includeTree, ...)
    mm <- NROW(bold_df) > 1

    if (!is.data.frame(bold_df)) {
      boldid <- NA_character_
      att <- "not found"
    } else {
      if (all(names(bold_df) == "input")) {
        boldid <- NA_character_
        att <- "not found"
      } else {
        bold_df <- rename(bold_df, replace = c('parentname' = 'parent',
          'tax_rank' = 'rank', 'tax_division' = 'division'),
          warn_missing = FALSE
        )
        cols <- c("taxid","taxon","rank","division","parentid","parent")
        to_add <- setdiff(cols, names(bold_df))
        if (length(to_add) > 0) {
          for (i in seq_along(to_add)) {
            bold_df <- cbind(bold_df, setNames(list(NA_character_), to_add[i]))
          }
        }

        bold_df <- bold_df[, c("taxid","taxon","rank","division","parentid","parent")]

        # should return NA if spec not found
        if (nrow(bold_df) == 0) {
          mssg(messages, m_not_found_sp_altclass)
          boldid <- NA_character_
          att <- 'not found'
        }
        # take the one tsn from data.frame
        if (nrow(bold_df) == 1) {
          boldid <- bold_df$taxid
          name <- bold_df$taxon
          rank_taken <- bold_df$rank
          att <- 'found'
        }
        # check for direct match
        if (nrow(bold_df) > 1) {
          names(bold_df)[grep('taxon', names(bold_df))] <- "target"
          di_rect <- match(tolower(bold_df$target), tolower(sci[i]))
          if (length(di_rect) == 1) {
            if (!all(is.na(di_rect))) {
              boldid <- bold_df$taxid[!is.na(di_rect)]
              name <- bold_df$target[!is.na(di_rect)]
              rank_taken <- bold_df$rank[!is.na(di_rect)]
              direct <- TRUE
              att <- 'found'
            } else {
              boldid <- NA_character_
              att <- 'not found'
            }
          } else {
            direct <- FALSE
            boldid <- NA_character_
            att <- 'found'
          }
        }
        # multiple matches
        if (any(
          nrow(bold_df) > 1 && is.na(boldid) |
          nrow(bold_df) > 1 && att == "found" && length(boldid) > 1
        )) {
          names(bold_df)[grep('^taxon$', names(bold_df))] <- "target"

          if (!is.null(division) || !is.null(parent) || !is.null(rank)) {
            bold_df <- filt(bold_df, "division", division)
            bold_df <- filt(bold_df, "parent", parent)
            bold_df <- filt(bold_df, "rank", rank)

            if (NROW(bold_df) == 0) {
              boldid <- NA_character_
              att <- 'not found'
              warning("filters 'division', 'parent', or 'rank' gave no records\n",
                "  check spelling or try a different filter value")
            }
          }

          if (NROW(bold_df) > 0) {
            bold_df <- sub_rows(bold_df, rows)
            boldid <- id <- bold_df$taxid
            if (length(id) == 1) {
              direct <- TRUE
              name <- bold_df$target
              rank_taken <- bold_df$rank
              att <- "found"
            }
          }

          if (ask) {
            # user prompt
            if (NROW(bold_df) > 0) {
              bold_df <- bold_df[order(bold_df$target), ]
              rownames(bold_df) <- 1:nrow(bold_df)
            }
            if (length(boldid) > 1 || NROW(bold_df) > 1) {
              # prompt
              message("\n\n")
              print(bold_df)
              message("\nMore than one TSN found for taxon '", sci[i], "'!\n
            Enter rownumber of taxon (other inputs will return 'NA'):\n") # prompt
              take <- scan(n = 1, quiet = TRUE, what = 'raw')

              if (length(take) == 0)
                take <- 'notake'
              if (take %in% seq_len(nrow(bold_df))) {
                take <- as.numeric(take)
                message("Input accepted, took taxon '",
                  as.character(bold_df$target[take]), "'.\n")
                boldid <-  bold_df$taxid[take]
                name <- bold_df$target[take]
                rank_taken <- bold_df$rank[take]
                att <- "found"
              } else {
                boldid <- NA_character_
                mssg(messages, "\nReturned 'NA'!\n\n")
                att <- "not found"
              }
            }
          } else {
            if (length(boldid) == 1) {
              att <- "found"
            } else {
              warning(sprintf(m_more_than_one_found, "bold", sci[i]),
                call. = FALSE)
              boldid <- NA_character_
              att <- m_na_ask_false
            }
          }
        }
      }
    }
    res <- list(id = as.character(boldid), name = name, rank = rank_taken,
      att = att, multiple = mm, direct = direct)
    prog$completed(sci[i], att)
    prog$prog(att)
    tstate$add(sci[i], res)
  }
  out <- tstate$get()
  res <- make_taxa_taxon(out, "bold")
  on.exit(prog$prog_summary(), add = TRUE)
  on.exit(tstate$exit, add = TRUE)
  return(res)
}
#' @export
#' @rdname get_bold
get_boldid <- function(...) {
  fchk("get_boldid", "get_bold")
  get_bold(...)
}

#' @export
#' @rdname get_bold
as_bold <- function(x, check=TRUE) UseMethod("as_bold")

#' @export
#' @rdname get_bold
as_bold.bold <- function(x, check=TRUE) x

#' @export
#' @rdname get_bold
as_bold.character <- function(x, check=TRUE) {
  if (length(x) == 1) 
    make_bold(x, check) 
  else 
    collapse(x, make_bold, "bold", check=check)
}

#' @export
#' @rdname get_bold
as_bold.list <- function(x, check=TRUE) {
  if (length(x) == 1) 
    make_bold(x, check) 
  else 
    collapse(x, make_bold, "bold", check=check)
}

#' @export
#' @rdname get_bold
as_bold.numeric <- function(x, check=TRUE) as_bold(as.character(x), check)

#' @export
#' @rdname get_bold
as_bold.data.frame <- function(x, check=TRUE) as_txid_df(x, check)

make_bold <- function(x, check=TRUE) {
  make_generic(x,
    'http://boldsystems.org/index.php/Taxbrowser_Taxonpage?taxid=%s',
    "bold", check)
}

check_bold <- function(x){
  tryid <- bold_tax_id(x)
  !identical("noresults", names(tryid)[2])
}

#' @export
#' @rdname get_bold
get_bold_ <- function(sci, messages = TRUE, fuzzy = FALSE,
  dataTypes='basic', includeTree=FALSE, rows = NA, searchterm = NULL, ...) {

  pchk(searchterm, "sci")
  stats::setNames(lapply(sci, get_bold_help, messages = messages,
    fuzzy = fuzzy, dataTypes=dataTypes, includeTree=includeTree,
    rows = rows, ...), sci)
}
#' @export
#' @rdname get_bold
get_boldid_ <- function(...) {
  fchk("get_boldid_", "get_bold_")
  get_bold_(...)
}

get_bold_help <- function(sci, messages, fuzzy, dataTypes,
  includeTree, rows, ...){
  
  mssg(messages, "\nRetrieving data for taxon '", sci, "'\n")
  df <- bold_search(name = sci, fuzzy = fuzzy, dataTypes = dataTypes,
    includeTree = includeTree)
  if(NROW(df) == 0) NULL else sub_rows(df, rows)
}
