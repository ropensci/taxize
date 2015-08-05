#' Get the EOL ID from Encyclopedia of Life from taxonomic names.
#'
#' Note that EOL doesn't expose an API endpoint for directly querying for EOL
#' taxon ID's, so we first use the function \code{\link[taxize]{eol_search}} to find pages
#' that deal with the species of interest, then use \code{\link[taxize]{eol_pages}}
#' to find the actual taxon IDs.
#'
#' @param sciname character; scientific name.
#' @param ask logical; should get_eolid be run in interactive mode?
#' If TRUE and more than one ID is found for the species, the user is asked for
#' input. If FALSE NA is returned for multiple matches.
#' @param key API key
#' @param ... Further args passed on to eol_search()
#' @param verbose logical; If TRUE the actual taxon queried is printed on the console.
#' @param rows numeric; Any number from 1 to inifity. If the default NA, all rows are considered.
#' Note that this function still only gives back a eolid class object with one to many identifiers.
#' See \code{\link[taxize]{get_eolid_}} to get back all, or a subset, of the raw data that you are
#' presented during the ask process.
#' @param x Input to \code{\link{as.eolid}}
#' @param check logical; Check if ID matches any existing on the DB, only used in
#' \code{\link{as.eolid}}
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
#' @examples \dontrun{
#' get_eolid(sciname='Pinus contorta')
#' get_eolid(sciname='Puma concolor')
#'
#' get_eolid(c("Puma concolor", "Pinus contorta"))
#'
#' # specify rows to limit choices available
#' get_eolid('Poa annua')
#' get_eolid('Poa annua', rows=1)
#' get_eolid('Poa annua', rows=2)
#' get_eolid('Poa annua', rows=1:2)
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
#' ## dont check, much faster
#' as.eolid("24954444", check=FALSE)
#' as.eolid(24954444, check=FALSE)
#' as.eolid(c("24954444","51389511","57266265"), check=FALSE)
#' as.eolid(list("24954444","51389511","57266265"), check=FALSE)
#'
#' (out <- as.eolid(c(24954444,51389511,57266265)))
#' data.frame(out)
#' as.eolid( data.frame(out) )
#'
#' # Get all data back
#' get_eolid_("Poa annua")
#' get_eolid_("Poa annua", rows=2)
#' get_eolid_("Poa annua", rows=1:2)
#' get_eolid_(c("asdfadfasd","Pinus contorta"))
#' }

get_eolid <- function(sciname, ask = TRUE, verbose = TRUE, key = NULL, rows = NA, ...){
  fun <- function(sciname, ask, verbose, rows, ...) {
    mssg(verbose, "\nRetrieving data for taxon '", sciname, "'\n")
    tmp <- eol_search(terms = sciname, key, ...)

    ms <- "Not found. Consider checking the spelling or alternate classification"
    datasource <- NA
    if (all(is.na(tmp))) {
      mssg(verbose, ms)
      id <- NA
      att <- "not found"
    } else {
      pageids <- tmp[grep(tolower(sciname), tolower(tmp$name)), "pageid"]

      if (length(pageids) == 0) {
        if (nrow(tmp) > 0)
        mssg(verbose, paste(ms, sprintf('\nDid find: %s', paste(tmp$name, collapse = "; "))))
        id <- NA
      } else {
        dfs <- lapply(pageids, function(x) eol_pages(x)$scinames)
        names(dfs) <- pageids
        dfs <- tc(dfs)
        if (length(dfs) > 1) dfs <- dfs[!sapply(dfs, nrow) == 0]
        df <- ldply(dfs)
#         df <- dfs[,c('.id','identifier','scientificname','nameaccordingto')]
#         names(df) <- c('pageid','eolid','name','source')
        df <- rename(df, c('.id' = 'pageid', 'identifier' = 'eolid',
                     'scientificname' = 'name', 'nameaccordingto' = 'source',
                     'taxonrank' = 'rank'))
        df <- getsourceshortnames(df)
        df$source <- as.character(df$source)
        df <- sub_rows(df, rows)

        if (nrow(df) == 0) {
          mssg(verbose, ms)
          id <- NA
        } else{
          id <- df$eolid
        }
        names(id) <- df$pageid
      }
    }

    # not found on eol
    if (length(id) == 0) {
      mssg(verbose, ms)
      id <- NA
      att <- 'not found'
    }
    # only one found on eol
    if (length(id) == 1 & !all(is.na(id))) {
      id <- df$eolid
      datasource <- df$source
      att <- 'found'
    }
    # more than one found on eol -> user input
    if (length(id) > 1) {
      if (ask) {
        rownames(df) <- 1:nrow(df)
        # prompt
        message("\n\n")
        message("\nMore than one eolid found for taxon '", sciname, "'!\n
            Enter rownumber of taxon (other inputs will return 'NA'):\n")
        print(df)
        take <- scan(n = 1, quiet = TRUE, what = 'raw')

        if (length(take) == 0) {
          take <- 'notake'
        }
        if (take %in% seq_len(nrow(df))) {
          take <- as.numeric(take)
          message("Input accepted, took eolid '", as.character(df$eolid[take]), "'.\n")
          id <- as.character(df$eolid[take])
          names(id) <- as.character(df$pageid[take])
          datasource <- as.character(df$source[take])
          att <- 'found'
        } else {
          id <- NA
          att <- 'not found'
          mssg(verbose, "\nReturned 'NA'!\n\n")
        }
      } else{
        id <- NA
        att <- "NA due to ask=FALSE"
      }
    }
    list(id = id, source = datasource, att = att)
  }
  sciname <- as.character(sciname)
  out <- lapply(sciname, fun, ask = ask, verbose = verbose, rows = rows, ...)
  ids <- unname(sapply(out, "[[", "id"))
  sources <- sapply(out, "[[", "source")
  ids <- structure(ids, class = "eolid", provider = sources, match = pluck(out, "att", ""))
  add_uri(ids, 'http://eol.org/pages/%s/overview')
}

getsourceshortnames <- function(input){
  lookup <- data.frame(z = c('COL','ITIS','GBIF','NCBI','IUCN'),
                       b = c('Species 2000 & ITIS Catalogue of Life: April 2013',
                           'Integrated Taxonomic Information System (ITIS)',
                           'GBIF Nub Taxonomy',
                           'NCBI Taxonomy',
                           'IUCN Red List (Species Assessed for Global Conservation)'),
                       stringsAsFactors = FALSE)
  bb <- merge(input, lookup, by.x = "source", by.y = "b")[, -1]
  # names(bb)[4] <- "source"
  taxize_sort_df(rename(bb, c('z' = 'source')), "name")
}

taxize_sort_df <- function(data, vars = names(data)) {
  if (length(vars) == 0 || is.null(vars))
    return(data)
  data[do.call("order", data[, vars, drop = FALSE]), , drop = FALSE]
}

#' @export
#' @rdname get_eolid
as.eolid <- function(x, check=TRUE) {
  UseMethod("as.eolid")
}

#' @export
#' @rdname get_eolid
as.eolid.eolid <- function(x, check=TRUE) {
  x
}

#' @export
#' @rdname get_eolid
as.eolid.character <- function(x, check=TRUE) {
  if (length(x) == 1) make_eolid(x, check) else collapse(x, make_eolid, "eolid", check = check)
}

#' @export
#' @rdname get_eolid
as.eolid.list <- function(x, check=TRUE) {
  if (length(x) == 1) make_eolid(x, check) else collapse(x, make_eolid, "eolid", check = check)
}

#' @export
#' @rdname get_eolid
as.eolid.numeric <- function(x, check=TRUE) {
  as.eolid(as.character(x), check)
}

#' @export
#' @rdname get_eolid
as.eolid.data.frame <- function(x, check=TRUE) {
  structure(x$ids, class = "eolid", match = x$match, uri = x$uri)
}

#' @export
#' @rdname get_eolid
as.data.frame.eolid <- function(x, ...){
  data.frame(ids = as.character(unclass(x)),
             class = "eolid",
             match = attr(x, "match"),
             uri = attr(x, "uri"),
             stringsAsFactors = FALSE)
}

make_eolid <- function(x, check=TRUE) make_generic(x, 'http://eol.org/pages/%s/overview', "eolid", check)

check_eolid <- function(x){
  url <- sprintf("http://eol.org/api/hierarchy_entries/1.0/%s.json", x)
  tryid <- GET(url)
  if (tryid$status_code == 200) TRUE else FALSE
}

#' @export
#' @rdname get_eolid
get_eolid_ <- function(sciname, verbose = TRUE, key = NULL, rows = NA, ...){
  setNames(lapply(sciname, get_eolid_help, verbose = verbose, key = key, rows = rows, ...), sciname)
}

get_eolid_help <- function(sciname, verbose, key, rows, ...){
  mssg(verbose, "\nRetrieving data for taxon '", sciname, "'\n")
  tmp <- eol_search(terms = sciname, key, ...)

  if (all(is.na(tmp))) {
    NULL
  } else {
    pageids <- tmp[grep(tolower(sciname), tolower(tmp$name)), "pageid"]
    if (length(pageids) == 0) {
      NULL
    } else {
      dfs <- lapply(pageids, function(x) eol_pages(x)$scinames)
      names(dfs) <- pageids
      dfs <- tc(dfs)
      if (length(dfs) > 1) dfs <- dfs[!sapply(dfs, nrow) == 0]
      dfs <- ldply(dfs)
      df <- dfs[,c('.id','identifier','scientificname','nameaccordingto')]
      names(df) <- c('pageid','eolid','name','source')
      df <- getsourceshortnames(df)
      df$source <- as.character(df$source)
      if (NROW(df) == 0) NULL else sub_rows(df, rows)
    }
  }
}
