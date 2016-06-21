#' Get the UK National Biodiversity Network ID from taxonomic names.
#'
#' @export
#' @param name character; scientific name.
#' @param ask logical; should get_nbnid be run in interactive mode?
#' If TRUE and more than one ID is found for the species, the user is asked for
#' input. If FALSE NA is returned for multiple matches.
#' @param verbose logical; If TRUE the actual taxon queried is printed on the
#' console.
#' @param rec_only (logical) If \code{TRUE} ids of recommended names are returned (i.e.
#' synonyms are removed). Defaults to \code{FALSE}. Remember, the id of a synonym is a
#' taxa with 'recommended' name status.
#' @param rank (character) If given, we attempt to limit the results to those taxa with the
#' matching rank.
#' @param rows numeric; Any number from 1 to inifity. If the default NA, all rows are considered.
#' Note that this function still only gives back a nbnid class object with one to many identifiers.
#' See \code{\link[taxize]{get_nbnid_}} to get back all, or a subset, of the raw data that you are
#' presented during the ask process.
#' @param ... Further args passed on to \code{nbn_search}
#' @param x Input to \code{\link{as.nbnid}}
#' @param check logical; Check if ID matches any existing on the DB, only used in
#' \code{\link{as.nbnid}}
#' @template getreturn
#'
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{get_uid}},
#' \code{\link[taxize]{get_tpsid}}, \code{\link[taxize]{get_eolid}},
#' \code{\link[taxize]{get_colid}}, \code{\link[taxize]{get_ids}},
#' \code{\link[taxize]{classification}}
#'
#' @author Scott Chamberlain, \email{myrmecocystus@@gmail.com}
#'
#' @examples \dontrun{
#' get_nbnid(name='Poa annua')
#' get_nbnid(name='Poa annua', rec_only=TRUE)
#' get_nbnid(name='Poa annua', rank='Species')
#' get_nbnid(name='Poa annua', rec_only=TRUE, rank='Species')
#' get_nbnid(name='Pinus contorta')
#'
#' # The NBN service handles common names too
#' get_nbnid(name='red-winged blackbird')
#'
#' # specify rows to limit choices available
#' get_nbnid('Poa annua')
#' get_nbnid('Poa annua', rows=1)
#' get_nbnid('Poa annua', rows=25)
#' get_nbnid('Poa annua', rows=1:2)
#'
#' # When not found
#' get_nbnid(name="uaudnadndj")
#' get_nbnid(c("Zootoca vivipara", "uaudnadndj"))
#' get_nbnid(c("Zootoca vivipara","Chironomus riparius", "uaudnadndj"))
#'
#' # Convert an nbnid without class information to a nbnid class
#' as.nbnid(get_nbnid("Zootoca vivipara")) # already a nbnid, returns the same
#' as.nbnid(get_nbnid(c("Zootoca vivipara","Pinus contorta"))) # same
#' as.nbnid('NHMSYS0001706186') # character
#' as.nbnid(c("NHMSYS0001706186","NHMSYS0000494848","NBNSYS0000010867")) # character vector, length > 1
#' as.nbnid(list("NHMSYS0001706186","NHMSYS0000494848","NBNSYS0000010867")) # list
#' ## dont check, much faster
#' as.nbnid('NHMSYS0001706186', check=FALSE)
#' as.nbnid(list("NHMSYS0001706186","NHMSYS0000494848","NBNSYS0000010867"), check=FALSE)
#'
#' (out <- as.nbnid(c("NHMSYS0001706186","NHMSYS0000494848","NBNSYS0000010867")))
#' data.frame(out)
#' as.nbnid( data.frame(out) )
#'
#' # Get all data back
#' get_nbnid_("Zootoca vivipara")
#' get_nbnid_("Poa annua", rows=2)
#' get_nbnid_("Poa annua", rows=1:2)
#' get_nbnid_(c("asdfadfasd","Pinus contorta"), rows=1:5)
#'
#' # use curl options
#' library("httr")
#' get_nbnid("Quercus douglasii", config=verbose())
#' bb <- get_nbnid("Quercus douglasii", config=progress())
#' }

get_nbnid <- function(name, ask = TRUE, verbose = TRUE, rec_only = FALSE, rank = NULL, rows = NA, ...){
  fun <- function(name, ask, verbose, rows) {
    direct <- FALSE
    mssg(verbose, "\nRetrieving data for taxon '", name, "'\n")
    df <- nbn_search(q = name, all = TRUE, ...)$data
    if (is.null(df)) df <- data.frame(NULL)
    mm <- NROW(df) > 1
    df <- sub_rows(df, rows)

    rank_taken <- NA
    if (nrow(df) == 0) {
      mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
      id <- NA_character_
      att <- 'not found'
    } else {
      if (rec_only) df <- df[ df$namestatus == 'Recommended', ]
      if (!is.null(rank)) df <- df[ tolower(df$rank) == tolower(rank), ]
      df <- df[,c('ptaxonversionkey','searchmatchtitle','rank','namestatus')]
      names(df)[1] <- 'nbnid'
      id <- df$nbnid
      rank_taken <- as.character(df$rank)
      att <- 'found'
    }

    # not found on NBN
    if (length(id) == 0) {
      mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
      id <- NA_character_
      att <- 'not found'
    }
    # more than one found -> user input
    if (length(id) > 1) {
      if (ask) {
        rownames(df) <- 1:nrow(df)
        # prompt
        message("\n\n")
        message("\nMore than one nbnid found for taxon '", name, "'!\n
            Enter rownumber of taxon (other inputs will return 'NA'):\n")
        print(df)
        take <- scan(n = 1, quiet = TRUE, what = 'raw')

        if (length(take) == 0) {
          take <- 'notake'
          att <- 'nothing chosen'
        }
        if (take %in% seq_len(nrow(df))) {
          take <- as.numeric(take)
          message("Input accepted, took nbnid '", as.character(df$nbnid[take]), "'.\n")
          id <- as.character(df$nbnid[take])
          rank_taken <- as.character(df$rank[take])
          att <- 'found'
        } else {
          id <- NA_character_
          att <- 'not found'
          mssg(verbose, "\nReturned 'NA'!\n\n")
        }
      } else{
        id <- NA_character_
        att <- 'NA due to ask=FALSE'
      }
    }
    list(id = id, rank = rank_taken, att = att, multiple = mm, direct = direct)
  }
  name <- as.character(name)
  out <- lapply(name, fun, ask = ask, verbose = verbose, rows = rows)
  ids <- pluck(out, "id", "")
  atts <- pluck(out, "att", "")
  ids <- structure(ids, class = "nbnid", match = atts,
                   multiple_matches = pluck(out, "multiple", logical(1)),
                   pattern_match = pluck(out, "direct", logical(1)))
  if ( !all(is.na(ids)) ) {
    urls <- sapply(out, function(z){
      if (!is.na(z[['id']]))
        sprintf('https://data.nbn.org.uk/Taxa/%s', z[['id']])
      else
        NA
    })
    attr(ids, 'uri') <- unlist(urls)
  }
  return(ids)
}

#' @export
#' @rdname get_nbnid
as.nbnid <- function(x, check=TRUE) UseMethod("as.nbnid")

#' @export
#' @rdname get_nbnid
as.nbnid.nbnid <- function(x, check=TRUE) x

#' @export
#' @rdname get_nbnid
as.nbnid.character <- function(x, check=TRUE) if(length(x) == 1) make_nbnid(x, check) else collapse(x, make_nbnid, "nbnid", check=check)

#' @export
#' @rdname get_nbnid
as.nbnid.list <- function(x, check=TRUE) if(length(x) == 1) make_nbnid(x, check) else collapse(x, make_nbnid, "nbnid", check=check)

#' @export
#' @rdname get_nbnid
as.nbnid.data.frame <- function(x, check=TRUE) {
  structure(x$ids, class="nbnid", match=x$match,
            multiple_matches = x$multiple_matches,
            pattern_match = x$pattern_match, uri=x$uri)
}

#' @export
#' @rdname get_nbnid
as.data.frame.nbnid <- function(x, ...){
  data.frame(ids = as.character(unclass(x)),
             class = "nbnid",
             match = attr(x, "match"),
             multiple_matches = attr(x, "multiple_matches"),
             pattern_match = attr(x, "pattern_match"),
             uri = attr(x, "uri"),
             stringsAsFactors = FALSE)
}

make_nbnid <- function(x, check=TRUE) make_generic(x, 'https://data.nbn.org.uk/Taxa/%s', "nbnid", check)

check_nbnid <- function(x){
  url <- "https://data.nbn.org.uk/api/taxa/"
  res <- GET(paste0(url, x))
  if ( res$status_code == 200 ) TRUE else FALSE
}

#' @export
#' @rdname get_nbnid
get_nbnid_ <- function(name, verbose = TRUE, rec_only = FALSE, rank = NULL, rows = NA, ...){
  setNames(lapply(name, get_nbnid_help, verbose = verbose, rec_only = rec_only, rank = rank, rows = rows, ...), name)
}

get_nbnid_help <- function(name, verbose, rec_only, rank, rows, ...){
  mssg(verbose, "\nRetrieving data for taxon '", name, "'\n")
  df <- nbn_search(q = name, all = TRUE, ...)$data
  if (is.null(df)) df <- data.frame(NULL)

  if (NROW(df) == 0) {
    NULL
  } else {
    if (rec_only) df <- df[ df$nameStatus == 'Recommended', ]
    if (!is.null(rank)) df <- df[ df$rank == rank, ]
    df <- df[,c('ptaxonversionkey', 'searchmatchtitle', 'rank', 'namestatus')]
    if (NROW(df) == 0) NULL else sub_rows(df, rows)
  }
}
