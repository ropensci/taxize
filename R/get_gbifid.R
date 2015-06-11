#' Get the GBIF backbone taxon ID from taxonomic names.
#'
#' @import plyr RCurl
#' @param sciname character; scientific name.
#' @param ask logical; should get_colid be run in interactive mode?
#' If TRUE and more than one ID is found for the species, the user is asked for
#' input. If FALSE NA is returned for multiple matches.
#' @param verbose logical; If TRUE the actual taxon queried is printed on the console.
#' @param rows numeric; Any number from 1 to inifity. If the default NA, all rows are considered.
#' Note that this function still only gives back a gbifid class object with one to many identifiers.
#' See \code{\link[taxize]{get_gbifid_}} to get back all, or a subset, of the raw data that you are
#' presented during the ask process.
#' @param phylum (character) A phylum (aka division) name. Optional. See \code{Filtering}
#' below.
#' @param class (character) A class name. Optional. See \code{Filtering} below.
#' @param order (character) An order name. Optional. See \code{Filtering} below.
#' @param family (character) A family name. Optional. See \code{Filtering} below.
#' @param rank (character) A taxonomic rank name. See \code{\link{rank_ref}} for possible
#' options. Though note that some data sources use atypical ranks, so inspect the
#' data itself for options. Optional. See \code{Filtering} below.
#' @param x Input to \code{\link{as.gbifid}}
#' @param check logical; Check if ID matches any existing on the DB, only used in
#' \code{\link{as.gbifid}}
#' @param ... Ignored
#'
#' @return A vector of unique identifiers. If a taxon is not found NA.
#' If more than one ID is found the function asks for user input.
#'
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{get_uid}},
#' \code{\link[taxize]{get_tpsid}}, \code{\link[taxize]{get_eolid}},
#' \code{\link[taxize]{get_colid}}
#'
#' @export
#' @author Scott Chamberlain, \email{myrmecocystus@@gmail.com}
#'
#' @details Internally in this function we use a function to search GBIF's taxonomy,
#' and if we find an exact match we return the ID for that match. If there isn't an
#' exact match we return the options to you to pick from.
#'
#' @section Filtering:
#' The parameters \code{phylum}, \code{class}, \code{order}, \code{family}, and \code{rank}
#' are not used in the search to the data provider, but are used in filtering the data down
#' to a subset that is closer to the target you want.  For all these parameters,
#' you can use regex strings since we use \code{\link{grep}} internally to match.
#'
#' @examples \dontrun{
#' get_gbifid(sciname='Poa annua')
#' get_gbifid(sciname='Pinus contorta')
#' get_gbifid(sciname='Puma concolor')
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
#' get_gbifid("Satyrium", phylum = "Magnoliophyta")
#' get_gbifid("Satyrium", phylum = "Arthropoda")
#' ### w/ phylum & rank
#' get_gbifid("Satyrium", phylum = "Arthropoda", rank = "genus")
#'
#' ## Rank example
#' get_gbifid("Poa")
#' get_gbifid("Poa", rank = "order")
#' get_gbifid("Poa", rank = "family")
#' get_gbifid("Poa", family = "Coccidae")
#'
#' # Fuzzy filter on any filtering fields
#' ## uses grep on the inside
#' get_gbifid("Satyrium", phylum = "arthropoda")
#' get_gbifid("Poa", order = "*tera")
#' get_gbifid("Poa", order = "*ales")
#'
#' # Convert a uid without class information to a uid class
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
#' as.gbifid(2704179, check=FALSE)
#' as.gbifid(c("2704179","2435099","3171445"), check=FALSE)
#' as.gbifid(list("2704179","2435099","3171445"), check=FALSE)
#'
#' (out <- as.gbifid(c(2704179,2435099,3171445)))
#' data.frame(out)
#' as.uid( data.frame(out) )
#'
#' # Get all data back
#' get_gbifid_("Puma concolor")
#' get_gbifid_(c("Pinus", "uaudnadndj"))
#' get_gbifid_(c("Pinus", "Puma"), rows=5)
#' get_gbifid_(c("Pinus", "Puma"), rows=1:5)
#' }

get_gbifid <- function(sciname, ask = TRUE, verbose = TRUE, rows = NA,
                       phylum = NULL, class = NULL, order = NULL,
                       family = NULL, rank = NULL){
  fun <- function(sciname, ask, verbose, rows) {
    mssg(verbose, "\nRetrieving data for taxon '", sciname, "'\n")
    df <- gbif_name_suggest(q = sciname, fields = c("key", "canonicalName", "rank",
                                                    "class", "phylum", "order", "family"))
    df <- sub_rows(df, rows)
    df <- rename(df, c('canonicalName' = 'canonicalname'))

    if (is.null(df))
      df <- data.frame(NULL)

    if (nrow(df) == 0) {
      mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
      id <- NA
      att <- "not found"
    } else {
      names(df)[1] <- 'gbifid'
      id <- df$gbifid
      att <- "found"
    }

    # not found
    if (length(id) == 0) {
      mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
      id <- NA
      att <- "not found"
    }

    # more than one found -> user input
    if (length(id) > 1) {
      # check for exact match
      matchtmp <- df[df$canonicalName %in% sciname, "gbifid"]
      if (length(matchtmp) == 1) {
        id <- as.character(matchtmp)
      } else {
        if (ask) {
          if (!is.null(phylum) || !is.null(class) || !is.null(order) ||
              !is.null(family) || !is.null(rank)) {
            df <- filt(df, "phylum", phylum)
            df <- filt(df, "class", class)
            df <- filt(df, "order", order)
            df <- filt(df, "family", family)
            df <- filt(df, "rank", rank)
            id <- df$gbifid
            if (length(id) == 1) {
              rank_taken <- as.character(df$rank)
              att <- "found"
            }
          }

          if (length(id) > 1) {
            # prompt
            message("\n\n")
            message("\nMore than one eolid found for taxon '", sciname, "'!\n
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
              message("Input accepted, took gbifid '", as.character(df$gbifid[take]), "'.\n")
              id <- as.character(df$gbifid[take])
              att <- "found"
            } else {
              id <- NA
              att <- "not found"
              mssg(verbose, "\nReturned 'NA'!\n\n")
            }
          }
        } else {
          id <- NA
          att <- "NA due to ask=FALSE"
        }
      }
    }
    c(id = id, att = att)
  }
  out <- lapply(as.character(sciname), fun, ask, verbose, rows)
  ids <- structure(sapply(out, "[[", "id"), class = "gbifid", match = sapply(out, "[[", "att"))
  add_uri(ids, 'http://www.gbif.org/species/%s')
}

gbif_name_suggest <- function(q=NULL, datasetKey=NULL, rank=NULL, fields=NULL, start=NULL,
                         limit=20, callopts=list()) {
  url = 'http://api.gbif.org/v1/species/suggest'
  args <- compact(list(q = q, rank = rank, offset = start, limit = limit))
  temp <- GET(url, query = args, callopts)
  stop_for_status(temp)
  tt <- content(temp)
  if (is.null(fields)) {
    toget <- c("key", "scientificName", "rank")
  } else {
    toget <- fields
  }
  matched <- sapply(toget, function(x) x %in% gbif_suggestfields())
  if (!any(matched))
    stop(sprintf("the fields %s are not valid", paste0(names(matched[matched == FALSE]), collapse = ",")))
  out <- lapply(tt, function(x) x[names(x) %in% toget])
  do.call(rbind.fill, lapply(out, data.frame))
}

gbif_suggestfields <- function() {
  c("key", "datasetTitle", "datasetKey", "nubKey", "parentKey", "parent",
    "kingdom", "phylum", "clazz", "order", "family", "genus", "species",
    "kingdomKey", "phylumKey", "classKey", "orderKey", "familyKey", "genusKey",
    "speciesKey", "scientificName", "canonicalName", "authorship",
    "accordingTo", "nameType", "taxonomicStatus", "rank", "numDescendants",
    "numOccurrences", "sourceId", "nomenclaturalStatus", "threatStatuses",
    "synonym")
}

#' @export
#' @rdname get_gbifid
as.gbifid <- function(x, check=FALSE) UseMethod("as.gbifid")

#' @export
#' @rdname get_gbifid
as.gbifid.gbifid <- function(x, check=FALSE) x

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
as.gbifid.data.frame <- function(x, check=TRUE) structure(x$ids, class="gbifid", match=x$match, uri=x$uri)

#' @export
#' @rdname get_gbifid
as.data.frame.gbifid <- function(x, ...){
  data.frame(ids = as.character(unclass(x)),
             class = "gbifid",
             match = attr(x, "match"),
             uri = attr(x, "uri"),
             stringsAsFactors = FALSE)
}

make_gbifid <- function(x, check=TRUE) make_generic(x, 'http://www.gbif.org/species/%s', "gbifid", check)

toid <- function(x, url, class){
  uri <- sprintf(url, x)
  structure(x, class=class, match="found", uri=uri)
}

check_gbifid <- function(x){
  tryid <- tryCatch(gbif_name_usage(key = x), error = function(e) e)
  if( "error" %in% class(tryid) && is.null(tryid$key) ) FALSE else TRUE
}

#' @export
#' @rdname get_gbifid
get_gbifid_ <- function(sciname, verbose = TRUE, rows = NA){
  setNames(lapply(sciname, get_gbifd_help, verbose = verbose, rows = rows), sciname)
}

get_gbifd_help <- function(sciname, verbose, rows){
  mssg(verbose, "\nRetrieving data for taxon '", sciname, "'\n")
  df <- gbif_name_suggest(q=sciname, fields = c("key","canonicalName","rank"))
  sub_rows(df, rows)
}
