#' Get the GBIF backbone taxon ID from taxonomic names.
#'
#' @import plyr RCurl
#' @param sciname character; scientific name.
#' @param ask logical; should get_colid be run in interactive mode?
#' If TRUE and more than one ID is found for the species, the user is asked for
#' input. If FALSE NA is returned for multiple matches.
#' @param verbose logical; If TRUE the actual taxon queried is printed on the console.
#' @param x Input to \code{\link{as.gbifid}}
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
#' @examples \donttest{
#' get_gbifid(sciname='Poa annua')
#' get_gbifid(sciname='Pinus contorta')
#' get_gbifid(sciname='Puma concolor')
#'
#' get_gbifid(c("Poa annua", "Pinus contorta"))
#'
#' # When not found, NA given
#' get_gbifid(sciname="uaudnadndj")
#' get_gbifid(c("Chironomus riparius", "uaudnadndj"))
#'
#' # Convert a uid without class information to a uid class
#' as.gbifid(get_gbifid("Poa annua")) # already a uid, returns the same
#' as.gbifid(get_gbifid(c("Poa annua","Puma concolor"))) # same
#' as.gbifid(2704179) # numeric
#' as.gbifid(c(2704179,2435099,3171445)) # numeric vector, length > 1
#' as.gbifid("2704179") # character
#' as.gbifid(c("2704179","2435099","3171445")) # character vector, length > 1
#' as.gbifid(list("2704179","2435099","3171445")) # list, either numeric or character
#' }

get_gbifid <- function(sciname, ask = TRUE, verbose = TRUE){
  fun <- function(sciname, ask, verbose) {
    mssg(verbose, "\nRetrieving data for taxon '", sciname, "'\n")
    df <- gbif_name_suggest(q=sciname, fields = c("key","canonicalName","rank"))

    if(is.null(df))
      df <- data.frame(NULL)

    if(nrow(df)==0){
      mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
      id <- NA
      att <- "not found"
    } else
    {
      names(df)[1] <- 'gbifid'
      id <- df$gbifid
      att <- "found"
    }

    # not found
    if(length(id) == 0){
      mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
      id <- NA
      att <- "not found"
    }

    # more than one found -> user input
    if(length(id) > 1){
      # check for exact match
      matchtmp <- df[df$canonicalName %in% sciname, "gbifid"]
      if(length(matchtmp) == 1){
        id <- as.character(matchtmp)
      } else
      {
        if(ask){
          rownames(df) <- 1:nrow(df)
          # prompt
          message("\n\n")
          message("\nMore than one eolid found for taxon '", sciname, "'!\n
            Enter rownumber of taxon (other inputs will return 'NA'):\n")
          print(df)
          take <- scan(n = 1, quiet = TRUE, what = 'raw')

          if(length(take) == 0){
            take <- 'notake'
            att <- 'nothing chosen'
          }
          if(take %in% seq_len(nrow(df))){
            take <- as.numeric(take)
            message("Input accepted, took gbifid '", as.character(df$gbifid[take]), "'.\n")
            id <- as.character(df$gbifid[take])
            att <- "found"
          } else {
            id <- NA
            att <- "not found"
            mssg(verbose, "\nReturned 'NA'!\n\n")
          }
        } else{
          id <- NA
          att <- "NA due to ask=FALSE"
        }
      }
    }
    c(id=id, att=att)
  }
  sciname <- as.character(sciname)
  out <- lapply(sciname, fun, ask, verbose)
  ids <- sapply(out, "[[", "id")
  atts <- sapply(out, "[[", "att")
  ids <- structure(ids, class="gbifid", match=atts)
  add_uri(ids, 'http://www.gbif.org/species/%s')
}

add_uri <- function(ids, url){
  if( !all(is.na(ids)) ){
    attr(ids, 'uri') <- sapply(ids, function(x){
      if(!is.na(x)) sprintf(url, x) else NA
    }, USE.NAMES = FALSE)
  }
  ids
}

gbif_name_suggest <- function(q=NULL, datasetKey=NULL, rank=NULL, fields=NULL, start=NULL,
                         limit=20, callopts=list())
{
  url = 'http://api.gbif.org/v1/species/suggest'
  args <- compact(list(q=q, rank=rank, offset=start, limit=limit))
  temp <- GET(url, query=args, callopts)
  stop_for_status(temp)
  tt <- content(temp)
  if(is.null(fields)){
    toget <- c("key","scientificName","rank")
  } else { toget <- fields }
  matched <- sapply(toget, function(x) x %in% gbif_suggestfields())
  if(!any(matched))
    stop(sprintf("the fields %s are not valid", paste0(names(matched[matched == FALSE]),collapse=",")))
  out <- lapply(tt, function(x) x[names(x) %in% toget])
  do.call(rbind.fill, lapply(out,data.frame))
}

gbif_suggestfields <- function(){
  c("key","datasetTitle","datasetKey","nubKey","parentKey","parent",
    "kingdom","phylum","clazz","order","family","genus","species",
    "kingdomKey","phylumKey","classKey","orderKey","familyKey","genusKey",
    "speciesKey","scientificName","canonicalName","authorship",
    "accordingTo","nameType","taxonomicStatus","rank","numDescendants",
    "numOccurrences","sourceId","nomenclaturalStatus","threatStatuses",
    "synonym")
}

#' @export
#' @rdname get_gbifid
as.gbifid <- function(x) UseMethod("as.gbifid")

#' @export
#' @rdname get_gbifid
as.gbifid.gbifid <- function(x) x

#' @export
#' @rdname get_gbifid
as.gbifid.character <- function(x) if(length(x) == 1) make_gbifid(x) else collapse(x, make_gbifid, "gbifid")

#' @export
#' @rdname get_gbifid
as.gbifid.list <- function(x) if(length(x) == 1) make_gbifid(x) else collapse(x, make_gbifid, "gbifid")

#' @export
#' @rdname get_gbifid
as.gbifid.numeric <- function(x) as.gbifid(as.character(x))

make_gbifid <- function(x){
  if(check_gbifid(x)){
    uri <- sprintf('http://www.gbif.org/species/%s', x)
    structure(x, class="gbfid", match="found", uri=uri)
  } else { structure(x, class="gbfid", match="not found")   }
}

check_gbifid <- function(x){
  tryid <- tryCatch(gbif_name_usage(key = x), error = function(e) e)
  if( "error" %in% class(tryid) && is.null(tryid$key) ) FALSE else TRUE
}
