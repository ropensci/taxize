#' Get the EOL ID from Encyclopedia of Life from taxonomic names.
#'
#' Note that EOL doesn't expose an API endpoint for directly querying for EOL
#' taxon ID's, so we first use the function \code{\link[taxize]{eol_search}} to find pages
#' that deal with the species of interest, then use \code{\link[taxize]{eol_pages}}
#' to find the actual taxon IDs.
#'
#' @import plyr RCurl
#' @param sciname character; scientific name.
#' @param ask logical; should get_eolid be run in interactive mode?
#' If TRUE and more than one ID is found for the species, the user is asked for
#' input. If FALSE NA is returned for multiple matches.
#' @param key API key
#' @param ... Further args passed on to eol_search()
#' @param verbose logical; If TRUE the actual taxon queried is printed on the console.
#' @param x Input to \code{\link{as.eolid}}
#'
#' @return A vector of unique identifiers (EOL). If a taxon is not found NA.
#' If more than one ID is found the function asks for user input.
#'
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{get_uid}},
#' \code{\link[taxize]{get_tpsid}}
#'
#' @export
#' @author Scott Chamberlain, \email{myrmecocystus@@gmail.com}
#'
#' @examples \donttest{
#' get_eolid(sciname='Pinus contorta')
#' get_eolid(sciname='Puma concolor')
#'
#' get_eolid(c("Puma concolor", "Pinus contorta"))
#'
#' # When not found
#' get_eolid(sciname="uaudnadndj")
#' get_eolid(c("Chironomus riparius", "uaudnadndj"))
#'
#' # Convert a eolid without class information to a eolid class
#' as.eolid(get_eolid("Chironomus riparius")) # already a eolid, returns the same
#' as.eolid(get_eolid(c("Chironomus riparius","Pinus contorta"))) # same
#' as.eolid(24954444) # numeric
#' as.eolid(c(24954444,51389511,57266265)) # numeric vector, length > 1
#' as.eolid("24954444") # character
#' as.eolid(c("24954444","51389511","57266265")) # character vector, length > 1
#' as.eolid(list("24954444","51389511","57266265")) # list, either numeric or character
#' }

get_eolid <- function(sciname, ask = TRUE, verbose = TRUE, key = NULL, ...){
  fun <- function(sciname, ask, verbose) {
    mssg(verbose, "\nRetrieving data for taxon '", sciname, "'\n")
    tmp <- eol_search(terms = sciname, key, ...)

    ms <- "Not found. Consider checking the spelling or alternate classification"
    datasource <- NA
    if(all(is.na(tmp))){
      mssg(verbose, ms)
      id <- NA
    } else {
      pageids <- tmp[grep(tolower(sciname), tolower(tmp$name)), "pageid"]

      if(length(pageids) == 0){
        if(nrow(tmp)>0)
        mssg(verbose, paste(ms, sprintf('\nDid find: %s', paste(tmp$name, collapse = "; "))))
        id <- NA
      } else
      {
        dfs <- lapply(pageids, function(x) eol_pages(x)$scinames)
        names(dfs) <- pageids
        dfs <- taxize_compact(dfs)
        if(length(dfs)>1) dfs <- dfs[!sapply(dfs, nrow)==0]
        dfs <- ldply(dfs)
        df <- dfs[,c('.id','identifier','scientificname','nameaccordingto')]
        names(df) <- c('pageid','eolid','name','source')
        df <- getsourceshortnames(df)

        if(nrow(df) == 0){
          mssg(verbose, ms)
          id <- NA
        } else{
          id <- df$eolid
        }
        names(id) <- df$pageid
      }
    }

    # not found on eol
    if(length(id) == 0){
      mssg(verbose, ms)
      id <- NA
    }
    # only one found on eol
    if(length(id) == 1 & !all(is.na(id))){
      id <- df$eolid
      datasource <- df$source
    }
    # more than one found on eol -> user input
    if(length(id) > 1){
      if(ask){
        rownames(df) <- 1:nrow(df)
        # prompt
        message("\n\n")
        message("\nMore than one eolid found for taxon '", sciname, "'!\n
            Enter rownumber of taxon (other inputs will return 'NA'):\n")
        print(df)
        take <- scan(n = 1, quiet = TRUE, what = 'raw')

        if(length(take) == 0)
          take <- 'notake'
        if(take %in% seq_len(nrow(df))){
          take <- as.numeric(take)
          message("Input accepted, took eolid '", as.character(df$eolid[take]), "'.\n")
          id <- as.character(df$eolid[take])
          names(id) <- as.character(df$pageid[take])
          datasource <- as.character(df$source[take])
        } else {
          id <- NA
          mssg(verbose, "\nReturned 'NA'!\n\n")
        }
      } else{
        id <- NA
      }
    }

    id_source <- list(id=id, source=datasource)
    return( id_source )
  }
  sciname <- as.character(sciname)
  out <- lapply(sciname, fun, ask, verbose)
  justids <- sapply(out, "[[", "id")
  justsources <- sapply(out, "[[", "source")
  class(justids) <- "eolid"
  s_pids <- names(justids)
  out <- unname(justids)
  if(!is.na(justids[1])){
    s_pids <- s_pids[vapply(s_pids, nchar, 1) > 0]
    attr(out, 'uri') <-
      sprintf('http://eol.org/pages/%s/overview', s_pids)
  }
  attr(out, 'provider') <- justsources
  return(out)
}

getsourceshortnames <- function(input){
  lookup <- data.frame(z=c('COL','ITIS','GBIF','NCBI','IUCN'),
                       b=c('Species 2000 & ITIS Catalogue of Life: April 2013',
                           'Integrated Taxonomic Information System (ITIS)',
                           'GBIF Nub Taxonomy',
                           'NCBI Taxonomy',
                           'IUCN Red List (Species Assessed for Global Conservation)'))
  bb <- merge(input, lookup, by.x="source", by.y="b")[,-1]
  names(bb)[4] <- "source"
  taxize_sort_df(bb, "name")
}

taxize_sort_df <- function (data, vars = names(data))
{
  if (length(vars) == 0 || is.null(vars))
    return(data)
  data[do.call("order", data[, vars, drop = FALSE]), , drop = FALSE]
}

#' @export
#' @rdname get_eolid
as.eolid <- function(x) UseMethod("as.eolid")

#' @export
#' @rdname get_eolid
as.eolid.eolid <- function(x) x

#' @export
#' @rdname get_eolid
as.eolid.character <- function(x) if(length(x) == 1) make_eolid(x) else collapse(x, make_eolid, "eolid", FALSE)

#' @export
#' @rdname get_eolid
as.eolid.list <- function(x) if(length(x) == 1) make_eolid(x) else collapse(x, make_eolid, "eolid", FALSE)

#' @export
#' @rdname get_eolid
as.eolid.numeric <- function(x) as.eolid(as.character(x))

make_eolid <- function(x){
  if(check_eolid(x)){
    uri <- sprintf('http://eol.org/pages/%s/overview', x)
    structure(x, class="eolid", uri=uri)
  } else { structure(x, class="eolid")   }
}

check_eolid <- function(x){
  tryid <- tryCatch(eol_pages(x), error = function(e) e)
  if( "error" %in% class(tryid) && "scinames" %in% names(tryid) ) FALSE else TRUE
}
