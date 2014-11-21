#' Get the UK National Biodiversity Network ID from taxonomic names.
#'
#' @export
#'
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
#' @param ... Further args passed on to \code{nbn_search}
#' @param x Input to \code{\link{as.nbnid}}
#'
#' @return A vector of unique identifiers. If a taxon is not found NA.
#' If more than one ID is found the function asks for user input.
#'
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{get_uid}},
#' \code{\link[taxize]{get_tpsid}}, \code{\link[taxize]{get_eolid}}
#'
#' @author Scott Chamberlain, \email{myrmecocystus@@gmail.com}
#'
#' @examples \donttest{
#' get_nbnid(name='Poa annua')
#' get_nbnid(name='Poa annua', rec_only=TRUE)
#' get_nbnid(name='Poa annua', rank='Species')
#' get_nbnid(name='Poa annua', rec_only=TRUE, rank='Species')
#' get_nbnid(name='Pinus contorta')
#'
#' # The NBN service handles common names too
#' get_nbnid(name='red-winged blackbird')
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
#'
#' (out <- as.nbnid(c("NHMSYS0001706186","NHMSYS0000494848","NBNSYS0000010867")))
#' data.frame(out)
#' as.nbnid( data.frame(out) )
#' }

get_nbnid <- function(name, ask = TRUE, verbose = TRUE, rec_only = FALSE, rank = NULL, ...){
  fun <- function(name, ask, verbose) {
    mssg(verbose, "\nRetrieving data for taxon '", name, "'\n")
    df <- nbn_search(q = name, all = TRUE, ...)$data
    if(is.null(df)) df <- data.frame(NULL)

    rank_taken <- NA
    if(nrow(df)==0){
      mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
      id <- NA
      att <- 'not found'
    } else
    {
      if(rec_only) df <- df[ df$nameStatus == 'Recommended', ]
      if(!is.null(rank)) df <- df[ df$rank == rank, ]
      df <- df[,c('ptaxonVersionKey','searchMatchTitle','rank','nameStatus')]
      names(df)[1] <- 'nbnid'
      id <- df$nbnid
      rank_taken <- as.character(df$rank)
      att <- 'found'
    }

    # not found on NBN
    if(length(id) == 0){
      mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
      id <- NA
      att <- 'not found'
    }
    # more than one found -> user input
    if(length(id) > 1){
      if(ask){
        rownames(df) <- 1:nrow(df)
        # prompt
        message("\n\n")
        message("\nMore than one nbnid found for taxon '", name, "'!\n
            Enter rownumber of taxon (other inputs will return 'NA'):\n")
        print(df)
        take <- scan(n = 1, quiet = TRUE, what = 'raw')

        if(length(take) == 0){
          take <- 'notake'
          att <- 'nothing chosen'
        }
        if(take %in% seq_len(nrow(df))){
          take <- as.numeric(take)
          message("Input accepted, took nbnid '", as.character(df$nbnid[take]), "'.\n")
          id <- as.character(df$nbnid[take])
          rank_taken <- as.character(df$rank[take])
          att <- 'found'
        } else {
          id <- NA
          att <- 'not found'
          mssg(verbose, "\nReturned 'NA'!\n\n")
        }
      } else{
        id <- NA
        att <- 'NA due to ask=FALSE'
      }
    }
    return( c(id=id, rank=rank_taken, att=att) )
  }
  name <- as.character(name)
  out <- lapply(name, fun, ask=ask, verbose=verbose)
  ids <- sapply(out, "[[", "id")
  atts <- sapply(out, "[[", "att")
  ids <- structure(ids, class="nbnid", match=atts)
  if( !all(is.na(ids)) ){
    urls <- sapply(out, function(z){
      if(!is.na(z[['id']]))
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
as.nbnid <- function(x) UseMethod("as.nbnid")

#' @export
#' @rdname get_nbnid
as.nbnid.nbnid <- function(x) x

#' @export
#' @rdname get_nbnid
as.nbnid.character <- function(x) if(length(x) == 1) make_nbnid(x) else collapse(x, make_nbnid, "nbnid")

#' @export
#' @rdname get_nbnid
as.nbnid.list <- function(x) if(length(x) == 1) make_nbnid(x) else collapse(x, make_nbnid, "nbnid")

#' @export
#' @rdname get_nbnid
as.nbnid.data.frame <- function(x) structure(x$ids, class="nbnid", match=x$match, uri=x$uri)

#' @export
#' @rdname get_nbnid
as.data.frame.nbnid <- function(x, ...){
  data.frame(ids = as.character(unclass(x)),
             class = "nbnid",
             match = attr(x, "match"),
             uri = attr(x, "uri"),
             stringsAsFactors = FALSE)
}

make_nbnid <- function(x){
  if(check_nbnid(x)){
    uri <- sprintf('https://data.nbn.org.uk/Taxa/%s', x)
    structure(x, class="nbnid", match="found", uri=uri)
  } else { structure(NA, class="nbnid", match="not found", uri=NA)  }
}

check_nbnid <- function(x){
  url <- "https://data.nbn.org.uk/api/taxa/"
  res <- GET(paste0(url, x))
  if( res$status_code == 200 ) TRUE else FALSE
}

# print.nbnid <- function(x){
#   cat(sprintf("<nbnid> %s", x), sep = "\n")
#   cat(sprintf("   match: %s", attr(x, "match")), sep = "\n")
#   cat(sprintf("   uri: %s", attr(x, "uri")))
# }
