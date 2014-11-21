#' Get the UID codes from NCBI for taxonomic names.
#'
#' Retrieve the Unique Identifier (UID) of a taxon from NCBI taxonomy browser.
#'
#' @import plyr RCurl
#' @param sciname character; scientific name.
#' @param ask logical; should get_tsn be run in interactive mode?
#' If TRUE and more than one TSN is found for the species, the user is asked for
#' input. If FALSE NA is returned for multiple matches.
#' @param verbose logical; If TRUE the actual taxon queried is printed on the console.
#' @param rows numeric; Any number from 1 to inifity. If the default NA, all rows are considered.
#' Note that this function still only gives back a uid class object with one to many identifiers.
#' See \code{\link[taxize]{get_uid_]}} to get back all, or a subset, of the raw data that you are
#' presented during the ask process.
#' @param x Input to \code{\link{as.uid}}
#' @param ... Ignored
#'
#' @return A vector of unique identifiers (UID). If a taxon is not found NA.
#' If more than one UID is found the function asks for user input (if ask = TRUE),
#' otherwise returns NA. Comes with an attribute \emph{match} to investigate the
#' reason for NA (either 'not found', 'found' or if ask = FALSE 'multi match').
#' If \code{ask=FALSE} and \code{rows} does not equal NA, then a data.frame is
#' given back, but not of the uid class, which you can't pass on to other functions
#' as you normally can.
#'
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{classification}}
#'
#' @export
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#'
#' @examples \donttest{
#' get_uid(c("Chironomus riparius", "Chaetopteryx"))
#' get_uid(c("Chironomus riparius", "aaa vva"))
#'
#' # When not found
#' get_uid("howdy")
#' get_uid(c("Chironomus riparius", "howdy"))
#'
#' # multiple matches
#' get_uid('Dugesia') # user prompt needed
#' get_uid('Dugesia', rows=1) # 2 choices, so returns only 1 row, so no choices
#' get_uid('Dugesia', ask = FALSE) # returns NA for multiple matches
#' get_uid('Dugesia', ask = FALSE) # or specify rows to get data back
#'
#' # Go to a website with more info on the taxon
#' res <- get_uid("Chironomus riparius")
#' browseURL(attr(res, "uri"))
#'
#' # Convert a uid without class information to a uid class
#' as.uid(get_uid("Chironomus riparius")) # already a uid, returns the same
#' as.uid(get_uid(c("Chironomus riparius","Pinus contorta"))) # same
#' as.uid(315567) # numeric
#' as.uid(c(315567,3339,9696)) # numeric vector, length > 1
#' as.uid("315567") # character
#' as.uid(c("315567","3339","9696")) # character vector, length > 1
#' as.uid(list("315567","3339","9696")) # list, either numeric or character
#'
#' (out <- as.uid(c(315567,3339,9696)))
#' data.frame(out)
#' as.uid( data.frame(out) )
#' }

get_uid <- function(sciname, ask = TRUE, verbose = TRUE, rows = NA){
  fun <- function(sciname, ask, verbose, rows) {
    mssg(verbose, "\nRetrieving data for taxon '", sciname, "'\n")
    sciname <- gsub(" ", "+", sciname)
    searchurl <- paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=taxonomy&term=",
                       sciname, sep = "")
    # NCBI limits requests to three per second
    xml_result <- xmlParse(getURL(searchurl))
    Sys.sleep(0.33)
    uid <- xpathSApply(xml_result, "//IdList/Id", xmlValue)
    uid <- sub_vector(uid, rows)
    att <- 'found'
    # not found on ncbi
    if (length(uid) == 0){
      mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
      uid <- NA
      att <- 'not found'
    }
    # more than one found on ncbi -> user input
    if(length(uid) > 1){
      if(ask){
        baseurl <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=taxonomy"
        ID <- paste("ID=", paste(uid, collapse= ","), sep = "")
        searchurl <- paste(baseurl, ID, sep = "&")
        tt <- getURL(searchurl)
        ttp <- xmlTreeParse(tt, useInternalNodes = TRUE)
        df <- ldply(xmlToList(ttp), data.frame)
        df <- df[df$Item..attrs != 'String', c(2,5,7)]
        names(df) <- c("UID", "Rank", "Division")
        rownames(df) <- 1:nrow(df)
        # df <- get_rows(df, rows)

        # prompt
        message("\n\n")
        message("\nMore than one UID found for taxon '", sciname, "'!\n
            Enter rownumber of taxon (other inputs will return 'NA'):\n")
        print(df)
        take <- scan(n = 1, quiet = TRUE, what = 'raw')

        if(length(take) == 0){
          take <- 'notake'
          att <- 'nothing chosen'
        }
        if(take %in% seq_len(nrow(df))){
          take <- as.numeric(take)
          message("Input accepted, took UID '", as.character(df$UID[take]), "'.\n")
          uid <- as.character(df$UID[take])
          att <- 'found'
        } else {
          uid <- NA
          att <- 'not found'
          mssg(verbose, "\nReturned 'NA'!\n\n")
        }
      } else {
        uid <- NA
        att <- 'NA due to ask=FALSE'
      }
    }
    return(data.frame(uid, att, stringsAsFactors= FALSE))
  }
  sciname <- as.character(sciname)
  outd <- ldply(sciname, fun, ask, verbose, rows)
  out <- structure(outd$uid, class="uid", match=outd$att)
  add_uri(out, 'http://www.ncbi.nlm.nih.gov/taxonomy/%s')
}

check_rows <- function(x){
  stopifnot(is.numeric(x) || any(is.na(x)))
  x
  # if(length(x) == 1 && !any(is.na(x))) 1:x else x
}

sub_rows <- function(x, rows){
  rows <- check_rows(rows)
  if( any(is.na(rows)) ) x else x[rows,]
}

sub_vector <- function(x, rows){
  rows <- check_rows(rows)
  if( any(is.na(rows)) ) x else x[rows]
}

#' @export
#' @rdname get_uid
as.uid <- function(x) UseMethod("as.uid")

#' @export
#' @rdname get_uid
as.uid.uid <- function(x) x

#' @export
#' @rdname get_uid
as.uid.character <- function(x) if(length(x) == 1) make_uid(x) else collapse(x, make_uid, "uid")

#' @export
#' @rdname get_uid
as.uid.list <- function(x) if(length(x) == 1) make_uid(x) else collapse(x, make_uid, "uid")

#' @export
#' @rdname get_uid
as.uid.numeric <- function(x) as.uid(as.character(x))

#' @export
#' @rdname get_uid
as.uid.data.frame <- function(x) structure(x$ids, class="uid", match=x$match, uri=x$uri)

#' @export
#' @rdname get_uid
as.data.frame.uid <- function(x, ...){
  data.frame(ids = as.character(unclass(x)),
             class = "uid",
             match = attr(x, "match"),
             uri = attr(x, "uri"),
             stringsAsFactors = FALSE)
}

make_uid <- function(x){
  if(check_uid(x)){
    uri <- sprintf('http://www.ncbi.nlm.nih.gov/taxonomy/%s', x)
    structure(x, class="uid", match="found", uri=uri)
  } else { structure(NA, class="uid", match="not found", uri=NA)   }
}

check_uid <- function(x){
  url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=taxonomy&id="
  res <- GET(paste0(url, x))
  tt <- content(res)
  tryid <- xpathSApply(tt, "//Id", xmlValue)
  identical(x, tryid)
}
