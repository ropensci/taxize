#' Get the NameID codes from Tropicos for taxonomic names.
#'
#' @import plyr RCurl
#' @param sciname (character) One or more scientific name's as a vector or list.
#' @param ask logical; should get_tpsid be run in interactive mode?
#' If TRUE and more than one ID is found for the species, the user is asked for
#' input. If FALSE NA is returned for multiple matches.
#' @param verbose logical; If TRUE the actual taxon queried is printed on the console.
#' @param key Your API key; loads from .Rprofile.
#' @param rows numeric; Any number from 1 to inifity. If the default NA, all rows are considered.
#' Note that this function still only gives back a tpsid class object with one to many identifiers.
#' See \code{\link[taxize]{get_tpsid_}} to get back all, or a subset, of the raw data that you are
#' presented during the ask process.
#' @param ... Other arguments passed to \code{\link[taxize]{tp_search}}.
#' @param x Input to \code{\link{as.tpsid}}
#' @param check logical; Check if ID matches any existing on the DB, only used in
#' \code{\link{as.tpsid}}
#'
#' @return A vector of unique identifiers. If a taxon is not found NA.
#' If more than one ID is found the function asks for user input.
#'
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{get_tpsid}}
#'
#' @export
#' @author Scott Chamberlain, \email{myrmecocystus@@gmail.com}
#'
#' @examples \dontrun{
#' get_tpsid(sciname='Poa annua')
#' get_tpsid(sciname='Pinus contorta')
#'
#' get_tpsid(c("Poa annua", "Pinus contorta"))
#'
#' # specify rows to limit choices available
#' get_tpsid('Poa annua')
#' get_tpsid('Poa annua', rows=1)
#' get_tpsid('Poa annua', rows=25)
#' get_tpsid('Poa annua', rows=1:2)
#'
#' # When not found, NA given (howdy is not a species name, and Chrinomus is a fly)
#' get_tpsid("howdy")
#' get_tpsid(c("Chironomus riparius", "howdy"))
#'
#' # pass to classification function to get a taxonomic hierarchy
#' classification(get_tpsid(sciname='Poa annua'))
#'
#' # factor class names are converted to character internally
#' spnames <- as.factor(c("Poa annua", "Pinus contorta"))
#' class(spnames)
#' get_tpsid(spnames)
#'
#' # pass in a list, works fine
#' get_tpsid(list("Poa annua", "Pinus contorta"))
#'
#' # Convert a tpsid without class information to a tpsid class
#' as.tpsid(get_tpsid("Pinus contorta")) # already a tpsid, returns the same
#' as.tpsid(get_tpsid(c("Chironomus riparius","Pinus contorta"))) # same
#' as.tpsid(24900183) # numeric
#' as.tpsid(c(24900183,50150089,50079838)) # numeric vector, length > 1
#' as.tpsid("24900183") # character
#' as.tpsid(c("24900183","50150089","50079838")) # character vector, length > 1
#' as.tpsid(list("24900183","50150089","50079838")) # list, either numeric or character
#' ## dont check, much faster
#' as.tpsid("24900183", check=FALSE)
#' as.tpsid(24900183, check=FALSE)
#' as.tpsid(c("24900183","50150089","50079838"), check=FALSE)
#' as.tpsid(list("24900183","50150089","50079838"), check=FALSE)
#'
#' (out <- as.tpsid(c(24900183,50150089,50079838)))
#' data.frame(out)
#' as.tpsid( data.frame(out) )
#'
#' # Get all data back
#' get_tpsid_("Poa annua")
#' get_tpsid_("Poa annua", rows=2)
#' get_tpsid_("Poa annua", rows=1:2)
#' get_tpsid_(c("asdfadfasd","Pinus contorta"), rows=1:5)
#' }

get_tpsid <- function(sciname, ask = TRUE, verbose = TRUE, key = NULL, rows = NA, ...){
  fun <- function(sciname, ask, verbose, rows) {
    mssg(verbose, "\nRetrieving data for taxon '", sciname, "'\n")
    tmp <- tp_search(name = sciname, key=key, ...)
    tmp <- sub_rows(tmp, rows)

    if(names(tmp)[[1]] == 'error'){
      mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
      id <- NA
      att <- 'not found'
    } else
    {
      df <- tmp[,c('nameid','scientificname','rankabbreviation','nomenclaturestatusname')]
      names(df) <- c('tpsid','name','rank','status')
      id <- df$tpsid
      att <- 'found'
    }

    # not found on tropicos
    if(length(id) == 0){
      mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
      id <- NA
      att <- 'not found'
    }
    # more than one found on tropicos -> user input
    if(length(id) > 1){
      if(ask){
        rownames(df) <- 1:nrow(df)
        # prompt
        message("\n\n")
        message("\nMore than one tpsid found for taxon '", sciname, "'!\n
          Enter rownumber of taxon (other inputs will return 'NA'):\n")
        print(df)
        take <- scan(n = 1, quiet = TRUE, what = 'raw')

        if(length(take) == 0){
          take <- 'notake'
          att <- 'nothing chosen'
        }
        if(take %in% seq_len(nrow(df))){
          take <- as.numeric(take)
          message("Input accepted, took tpsid '", as.character(df$tpsid[take]), "'.\n")
          id <- as.character(df$tpsid[take])
          att <- 'found'
        } else {
          id <- NA
          mssg(verbose, "\nReturned 'NA'!\n\n")
          att <- 'not found'
        }
      } else{
        id <- NA
        att <- 'NA due to ask=FALSE'
      }
    }
    list(id=id, att=att)
  }
  sciname <- as.character(sciname)
  out <- lapply(sciname, fun, ask, verbose, rows)
  ids <- unlist(pluck(out, "id"))
  atts <- pluck(out, "att", "")
  ids <- structure(ids, class="tpsid", match=atts)
  add_uri(ids, 'http://tropicos.org/Name/%s')
}

#' @export
#' @rdname get_tpsid
as.tpsid <- function(x, check=TRUE) UseMethod("as.tpsid")

#' @export
#' @rdname get_tpsid
as.tpsid.tpsid <- function(x, check=TRUE) x

#' @export
#' @rdname get_tpsid
as.tpsid.character <- function(x, check=TRUE) if(length(x) == 1) make_tpsid(x, check) else collapse(x, make_tpsid, "tpsid", check=check)

#' @export
#' @rdname get_tpsid
as.tpsid.list <- function(x, check=TRUE) if(length(x) == 1) make_tpsid(x, check) else collapse(x, make_tpsid, "tpsid", check=check)

#' @export
#' @rdname get_tpsid
as.tpsid.numeric <- function(x, check=TRUE) as.tpsid(as.character(x), check)

#' @export
#' @rdname get_tpsid
as.tpsid.data.frame <- function(x, check=TRUE) structure(x$ids, class="tpsid", match=x$match, uri=x$uri)

#' @export
#' @rdname get_tpsid
as.data.frame.tpsid <- function(x, ...){
  data.frame(ids = as.character(unclass(x)),
             class = "tpsid",
             match = attr(x, "match"),
             uri = attr(x, "uri"),
             stringsAsFactors = FALSE)
}

make_tpsid <- function(x, check=TRUE) make_generic(x, 'http://tropicos.org/Name/%s', "tpsid", check)

check_tpsid <- function(x){
  res <- tp_summary(x)
  !identical(names(res), "error")
}

#' @export
#' @rdname get_tpsid
get_tpsid_ <- function(sciname, verbose = TRUE, key = NULL, rows = NA, ...){
  setNames(lapply(sciname, get_tpsid_help, verbose = verbose, key=key, rows = rows, ...), sciname)
}

get_tpsid_help <- function(sciname, verbose, key, rows, ...){
  mssg(verbose, "\nRetrieving data for taxon '", sciname, "'\n")
  df <- tp_search(name=sciname, key=key, ...)
  if("error" %in% names(df)) NULL else sub_rows(df, rows)
}
