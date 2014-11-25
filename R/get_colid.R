#' Get the Catalogue of Life ID from taxonomic names.
#'
#' @import plyr RCurl
#' @param sciname character; scientific name.
#' @param ask logical; should get_colid be run in interactive mode?
#' If TRUE and more than one ID is found for the species, the user is asked for
#' input. If FALSE NA is returned for multiple matches.
#' @param verbose logical; If TRUE the actual taxon queried is printed on the
#'    console.
#' @param rows numeric; Any number from 1 to inifity. If the default NA, all rows are considered.
#' Note that this function still only gives back a colid class object with one to many identifiers.
#' See \code{\link[taxize]{get_colid_}} to get back all, or a subset, of the raw data that you are
#' presented during the ask process.
#' @param x Input to as.colid
#' @param ... Ignored
#'
#' @return A vector of unique identifiers. If a taxon is not found NA.
#' If more than one ID is found the function asks for user input.
#'
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{get_colid}},
#' \code{\link[taxize]{get_tpsid}}, \code{\link[taxize]{get_eolid}}
#'
#' @export
#' @author Scott Chamberlain, \email{myrmecocystus@@gmail.com}
#'
#' @examples \donttest{
#' get_colid(sciname='Poa annua')
#' get_colid(sciname='Pinus contorta')
#' get_colid(sciname='Puma concolor')
#' get_colid(sciname="Abudefduf saxatilis")
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
#' # Convert a uid without class information to a uid class
#' as.colid(get_colid("Chironomus riparius")) # already a uid, returns the same
#' as.colid(get_colid(c("Chironomus riparius","Pinus contorta"))) # same
#' as.colid(8663146) # numeric
#' as.colid(c(8663146,19736162,18158318)) # numeric vector, length > 1
#' as.colid("19736162") # character
#' as.colid(c("8663146","19736162","18158318")) # character vector, length > 1
#' as.colid(list("8663146","19736162","18158318")) # list, either numeric or character
#'
#' (out <- as.colid(c(8663146,19736162,18158318)))
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
#' }

get_colid <- function(sciname, ask = TRUE, verbose = TRUE, rows = NA){
  fun <- function(sciname, ask, verbose, rows) {
    mssg(verbose, "\nRetrieving data for taxon '", sciname, "'\n")
    df <- col_search(name=sciname)[[1]]
    df <- sub_rows(df, rows)

    rank_taken <- NA
    if(NROW(df)==0){
      id <- NA
      att <- "not found"
    } else
    {
      df <- df[,c('id','name','rank','status','source','acc_name')]
      names(df)[1] <- 'colid'
      id <- df$colid
      rank_taken <- as.character(df$rank)
      att <- "found"
    }

    # not found on col
    if(all(is.na(id))){
      mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
      id <- NA
      att <- "not found"
    }
    # more than one found -> user input
    if(length(id) > 1){
      if(ask){
        rownames(df) <- 1:nrow(df)
        # prompt
        message("\n\n")
        message("\nMore than one colid found for taxon '", sciname, "'!\n
            Enter rownumber of taxon (other inputs will return 'NA'):\n")
        print(df)
        take <- scan(n = 1, quiet = TRUE, what = 'raw')

        if(length(take) == 0){
          take <- 'notake'
          att <- 'nothing chosen'
        }
        if(take %in% seq_len(nrow(df))){
          take <- as.numeric(take)
          message("Input accepted, took colid '", as.character(df$colid[take]), "'.\n")
          id <- as.character(df$colid[take])
          rank_taken <- as.character(df$rank[take])
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
    c(id=id, rank=rank_taken, att=att)
  }
  sciname <- as.character(sciname)
  out <- lapply(sciname, fun, ask=ask, verbose=verbose, rows=rows)
  ids <- sapply(out, "[[", "id")
  atts <- sapply(out, "[[", "att")
  ids <- structure(ids, class="colid", match=atts)
  if( !all(is.na(ids)) ){
    urls <- sapply(out, function(z){
      if(!is.na(z[['id']])){
        if(tolower(z['rank']) == "species"){
          sprintf('http://www.catalogueoflife.org/col/details/species/id/%s', z[['id']])
        } else {
          sprintf('http://www.catalogueoflife.org/col/browse/tree/id/%s', z[['id']])
        }
      } else { NA }
    })
    attr(ids, 'uri') <- unlist(urls)
  }
  return(ids)
}

#' @export
#' @rdname get_colid
as.colid <- function(x) UseMethod("as.colid")

#' @export
#' @rdname get_colid
as.colid.colid <- function(x) x

#' @export
#' @rdname get_colid
as.colid.character <- function(x) if(length(x) == 1) make_colid(x) else collapse(x, make_colid, "colid")

#' @export
#' @rdname get_colid
as.colid.list <- function(x) if(length(x) == 1) make_colid(x) else collapse(x, make_colid, "colid")

#' @export
#' @rdname get_colid
as.colid.numeric <- function(x) as.colid(as.character(x))

#' @export
#' @rdname get_colid
as.colid.data.frame <- function(x) structure(x$ids, class="colid", match=x$match, uri=x$uri)

#' @export
#' @rdname get_colid
as.data.frame.colid <- function(x, ...){
  data.frame(ids = as.character(unclass(x)),
             class = "colid",
             match = attr(x, "match"),
             uri = attr(x, "uri"),
             stringsAsFactors = FALSE)
}

make_colid <- function(x){
  if(check_colid(x)){
    uri <- sprintf('http://www.catalogueoflife.org/col/details/species/id/%s', x)
    structure(x, class="uid", match="found", uri=uri)
  } else { structure(NA, class="uid", match="not found", uri=NA)   }
}

check_colid <- function(x){
  url <- "http://www.catalogueoflife.org/col/details/species/id/"
  res <- GET(paste0(url, x))
  tt <- content(res)
  tryid <- xpathSApply(tt, '//p', xmlValue)
  identical(list(), tryid)
}

#' @export
#' @rdname get_colid
get_colid_ <- function(sciname, verbose = TRUE, rows = NA){
  setNames(lapply(sciname, get_colid_help, verbose = verbose, rows = rows), sciname)
}

get_colid_help <- function(sciname, verbose, rows){
  mssg(verbose, "\nRetrieving data for taxon '", sciname, "'\n")
  df <- col_search(name=sciname)[[1]]
  if(NROW(df) == 0) NULL else sub_rows(df, rows)
}
