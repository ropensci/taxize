#' Get the Catalogue of Life ID from taxonomic names.
#'
#' @export
#' @param sciname character; scientific name.
#' @param ask logical; should get_colid be run in interactive mode?
#' If TRUE and more than one ID is found for the species, the user is asked for
#' input. If FALSE NA is returned for multiple matches.
#' @param messages logical; If TRUE the actual taxon queried is printed on the
#'    console.
#' @param rows numeric; Any number from 1 to infinity. If the default NA, all rows are considered.
#' Note that this function still only gives back a colid class object with one to many identifiers.
#' See \code{\link[taxize]{get_colid_}} to get back all, or a subset, of the raw data that you are
#' presented during the ask process.
#' @param kingdom (character) A kingdom name. Optional. See \code{Filtering}
#' below.
#' @param phylum (character) A phylum (aka division) name. Optional. See \code{Filtering}
#' below.
#' @param class (character) A class name. Optional. See \code{Filtering} below.
#' @param order (character) An order name. Optional. See \code{Filtering} below.
#' @param family (character) A family name. Optional. See \code{Filtering} below.
#' @param rank (character) A taxonomic rank name. See \code{\link{rank_ref}} for possible
#' options. Though note that some data sources use atypical ranks, so inspect the
#' data itself for options. Optional. See \code{Filtering} below.
#' @param x Input to as.colid
#' @param ... Ignored
#' @param check logical; Check if ID matches any existing on the DB, only used in
#' \code{\link{as.colid}}
#' @template getreturn
#'
#' @section Filtering:
#' The parameters \code{kingdom}, \code{phylum}, \code{class}, \code{order}, \code{family},
#' and \code{rank} are not used in the search to the data provider, but are used in filtering
#' the data down to a subset that is closer to the target you want. For all these parameters,
#' you can use regex strings since we use \code{\link{grep}} internally to match.
#' Filtering narrows down to the set that matches your query, and removes the rest.
#'
#' @family taxonomic-ids
#' @seealso \code{\link[taxize]{classification}}
#'
#' @author Scott Chamberlain, \email{myrmecocystus@@gmail.com}
#'
#' @examples \dontrun{
#' get_colid(sciname='Poa annua')
#' get_colid(sciname='Pinus contorta')
#' get_colid(sciname='Puma concolor')
#' # get_colid(sciname="Abudefduf saxatilis")
#'
#' get_colid(c("Poa annua", "Pinus contorta"))
#'
#' # specify rows to limit choices available
#' get_colid(sciname='Poa annua')
#' get_colid(sciname='Poa annua', rows=1)
#' get_colid(sciname='Poa annua', rows=2)
#' get_colid(sciname='Poa annua', rows=1:2)
#'
#' # When not found
#' get_colid(sciname="uaudnadndj")
#' get_colid(c("Chironomus riparius", "uaudnadndj"))
#'
#' # Narrow down results to a division or rank, or both
#' ## Satyrium example
#' ### Results w/o narrowing
#' get_colid("Satyrium")
#' ### w/ division
#' get_colid("Satyrium", kingdom = "Plantae")
#' get_colid("Satyrium", kingdom = "Animalia")
#'
#' ## Rank example
#' get_colid("Poa")
#' get_colid("Poa", kingdom = "Plantae")
#' get_colid("Poa", kingdom = "Animalia")
#'
#' # Fuzzy filter on any filtering fields
#' ## uses grep on the inside
#' get_colid("Satyrium", kingdom = "p")
#'
#' # Convert a uid without class information to a uid class
#' as.colid(get_colid("Chironomus riparius")) # already a uid, returns the same
#' as.colid(get_colid(c("Chironomus riparius","Pinus contorta"))) # same
#' as.colid("714831352ad94741e4321eccdeb29f58") # character
#' # character vector, length > 1
#' as.colid(c("714831352ad94741e4321eccdeb29f58", "3b35900f74ff6e4b073ddb95c32b1f8d"))
#' # list, either numeric or character
#' as.colid(list("714831352ad94741e4321eccdeb29f58", "3b35900f74ff6e4b073ddb95c32b1f8d"))
#' ## dont check, much faster
#' as.colid("714831352ad94741e4321eccdeb29f58", check=FALSE)
#' as.colid(c("714831352ad94741e4321eccdeb29f58", "3b35900f74ff6e4b073ddb95c32b1f8d"),
#'  check=FALSE)
#' as.colid(list("714831352ad94741e4321eccdeb29f58", "3b35900f74ff6e4b073ddb95c32b1f8d"),
#'  check=FALSE)
#'
#' (out <- as.colid(c("714831352ad94741e4321eccdeb29f58", "3b35900f74ff6e4b073ddb95c32b1f8d")))
#' data.frame(out)
#' as.colid( data.frame(out) )
#'
#' # Get all data back
#' get_colid_("Poa annua")
#' get_colid_("Poa annua", rows=2)
#' get_colid_("Poa annua", rows=1:2)
#' get_colid_(c("asdfadfasd","Pinus contorta"))
#'
#' get_colid(sciname="Andropadus nigriceps fusciceps", rows=1)
#'
#' # use curl options
#' library("httr")
#' get_colid("Quercus douglasii", config=verbose())
#' bb <- get_colid("Quercus douglasii", config=progress())
#' }

get_colid <- function(sciname, ask = TRUE, messages = TRUE, rows = NA,
                      kingdom = NULL, phylum = NULL, class = NULL, order = NULL,
                      family = NULL, rank = NULL, ...){

  assert(ask, "logical")
  assert(messages, "logical")
  assert(kingdom, "character")
  assert(phylum, "character")
  assert(class, "character")
  assert(order, "character")
  assert(family, "character")
  assert(rank, "character")

  fun <- function(sciname, ask, messages, rows, ...) {
    direct <- FALSE
    mssg(messages, "\nRetrieving data for taxon '", sciname, "'\n")
    df <- col_search(name = sciname, response = "full", ...)[[1]]
    df <- df[, names(df) %in% c("name","rank","id","name_status","kingdom","family","acc_name")]
    mm <- NROW(df) > 1

    rank_taken <- NA
    if (NROW(df) == 0) {
      id <- NA_character_
      att <- "not found"
    } else {
      df <- stats::setNames(df, tolower(names(df)))
      df <- rename(df, c("id" = "colid"))
      colnames(df)[which(colnames(df) == 'id')] <- 'colid'
      id <- df$colid
      rank_taken <- as.character(df$rank)
      att <- "found"
    }

    # not found on col
    if (all(is.na(id))) {
      mssg(messages, "Not found. Consider checking the spelling or alternate classification")
      id <- NA_character_
      att <- "not found"
    }
    # more than one found -> user input
    if (length(id) > 1) {
      rownames(df) <- 1:nrow(df)

      if (!is.null(kingdom) || !is.null(phylum) || !is.null(class) ||
          !is.null(order) || !is.null(family) || !is.null(rank)) {
        df <- filt(df, "kingdom", kingdom)
        df <- filt(df, "phylum", phylum)
        df <- filt(df, "class", class)
        df <- filt(df, "order", order)
        df <- filt(df, "family", family)
        df <- filt(df, "rank", rank)
      }

      df <- sub_rows(df, rows)
      id <- df$colid
      if (length(id) == 1) {
        rank_taken <- as.character(df$rank)
        direct <- TRUE
        att <- "found"
      }

      if (length(id) > 1) {
        if (ask) {
          # prompt
          rownames(df) <- 1:nrow(df)
          message("\n\n")
          message("\nMore than one colid found for taxon '", sciname, "'!\n
            Enter rownumber of taxon (other inputs will return 'NA'):\n")
          print(df)
          take <- scan(n = 1, quiet = TRUE, what = 'raw')

          if (length(take) == 0) {
            take <- 'notake'
            att <- 'nothing chosen'
          }
          if (take %in% seq_len(nrow(df))) {
            take <- as.numeric(take)
            message("Input accepted, took colid '", as.character(df$colid[take]), "'.\n")
            id <- as.character(df$colid[take])
            rank_taken <- as.character(df$rank[take])
            att <- "found"
          } else {
            id <- NA_character_
            att <- "not found"
            mssg(messages, "\nReturned 'NA'!\n\n")
          }
        } else {
          if (length(id) != 1) {
            warning(
              sprintf("More than one id found for taxon '%s'; refine query or set ask=TRUE",
                      sciname),
              call. = FALSE
            )
            id <- NA_character_
            att <- 'NA due to ask=FALSE & > 1 result'
          }
        }
      }
    }
    list(id = id, rank = rank_taken, att = att, multiple = mm, direct = direct)
  }
  sciname <- as.character(sciname)
  out <- lapply(sciname, fun, ask = ask, messages = messages, rows = rows, ...)
  ids <- pluck(out, "id", "")
  atts <- pluck(out, "att", "")
  ids <- structure(ids, class = "colid", match = atts,
                   multiple_matches = pluck(out, "multiple", logical(1)),
                   pattern_match = pluck(out, "direct", logical(1)))
  if ( !all(is.na(ids)) ) {
    urls <- sapply(out, function(z){
      if (!is.na(z[['id']])) {
        if (tolower(z['rank']) == "species") {
          sprintf('http://www.catalogueoflife.org/col/details/species/id/%s', z[['id']])
        } else {
          sprintf('http://www.catalogueoflife.org/col/browse/tree/id/%s', z[['id']])
        }
      } else {
        NA
      }
    })
    attr(ids, 'uri') <- unlist(urls)
  }
  return(ids)
}

#' @export
#' @rdname get_colid
as.colid <- function(x, check=TRUE) UseMethod("as.colid")

#' @export
#' @rdname get_colid
as.colid.colid <- function(x, check=TRUE) x

#' @export
#' @rdname get_colid
as.colid.character <- function(x, check=TRUE) if (length(x) == 1) make_colid(x, check) else collapse(x, make_colid, "colid", check = check)

#' @export
#' @rdname get_colid
as.colid.list <- function(x, check=TRUE) if (length(x) == 1) make_colid(x, check) else collapse(x, make_colid, "colid", check = check)

#' @export
#' @rdname get_colid
as.colid.data.frame <- function(x, check=TRUE) {
  structure(x$ids, class = "colid", match = x$match,
            multiple_matches = x$multiple_matches,
            pattern_match = x$pattern_match, uri = x$uri)
}

#' @export
#' @rdname get_colid
as.data.frame.colid <- function(x, ...){
  data.frame(ids = as.character(unclass(x)),
             class = "colid",
             match = attr(x, "match"),
             multiple_matches = attr(x, "multiple_matches"),
             pattern_match = attr(x, "pattern_match"),
             uri = attr(x, "uri"),
             stringsAsFactors = FALSE)
}

make_colid <- function(x, check=TRUE) make_generic(x, 'http://www.catalogueoflife.org/col/details/species/id/%s', "colid", check)

check_colid <- function(x){
  url <- "http://www.catalogueoflife.org/col/details/species/id/"
  res <- GET(paste0(url, x))
  !grepl("Species not found", res)
}

#' @export
#' @rdname get_colid
get_colid_ <- function(sciname, messages = TRUE, rows = NA){
  setNames(lapply(sciname, get_colid_help, messages = messages, rows = rows), sciname)
}

get_colid_help <- function(sciname, messages, rows){
  mssg(messages, "\nRetrieving data for taxon '", sciname, "'\n")
  df <- col_search(name = sciname)[[1]]
  if (NROW(df) == 0) NULL else sub_rows(df, rows)
}
