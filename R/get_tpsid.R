#' Get the NameID codes from Tropicos for taxonomic names.
#'
#' @import plyr RCurl
#' @param sciname (character) One or more scientific name's as a vector or list.
#' @param ask logical; should get_tpsid be run in interactive mode?
#' If TRUE and more than one ID is found for the species, the user is asked for
#' input. If FALSE NA is returned for multiple matches.
#' @param verbose logical; If TRUE the actual taxon queried is printed on the console.
#' @param key Your API key; loads from .Rprofile.
#' @param ... Other arguments passed to \code{\link[taxize]{tp_search}}.
#' @param x Input to \code{\link{as.tpsid}}
#'
#' @return A vector of unique identifiers. If a taxon is not found NA.
#' If more than one ID is found the function asks for user input.
#'
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{get_tpsid}}
#'
#' @export
#' @author Scott Chamberlain, \email{myrmecocystus@@gmail.com}
#'
#' @examples \donttest{
#' get_tpsid(sciname='Poa annua')
#' get_tpsid(sciname='Pinus contorta')
#'
#' get_tpsid(c("Poa annua", "Pinus contorta"))
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
#' }

get_tpsid <- function(sciname, ask = TRUE, verbose = TRUE, key = NULL, ...){
  fun <- function(sciname, ask, verbose) {
    mssg(verbose, "\nRetrieving data for taxon '", sciname, "'\n")
    tmp <- tp_search(name = sciname, key=key, ...)

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
  out <- lapply(sciname, fun, ask, verbose)
  ids <- unlist(pluck(out, "id"))
  atts <- pluck(out, "att", "")
  ids <- structure(ids, class="tpsid", match=atts)
  if( !all(is.na(ids)) ){
    attr(ids, 'uri') <- sapply(ids, function(x){
      if(!is.na(x)) sprintf('http://tropicos.org/Name/%s', x) else NA
    }, USE.NAMES = FALSE)
  }
  return( ids )
}

#' @export
#' @rdname get_tpsid
as.tpsid <- function(x) UseMethod("as.tpsid")

#' @export
#' @rdname get_tpsid
as.tpsid.tpsid <- function(x) x

#' @export
#' @rdname get_tpsid
as.tpsid.character <- function(x) if(length(x) == 1) make_tpsid(x) else collapse(x, make_tpsid, "tpsid")

#' @export
#' @rdname get_tpsid
as.tpsid.list <- function(x) if(length(x) == 1) make_tpsid(x) else collapse(x, make_tpsid, "tpsid")

#' @export
#' @rdname get_tpsid
as.tpsid.numeric <- function(x) as.tpsid(as.character(x))

make_tpsid <- function(x){
  if(check_tpsid(x)){
    uri <- sprintf('http://tropicos.org/Name/%s', x)
    structure(x, class="tpsid", match="found", uri=uri)
  } else { structure(NA, class="tpsid", match="not found")   }
}

check_tpsid <- function(x){
  res <- tp_summary(x)
  !identical(names(res), "error")
}
