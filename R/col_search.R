#' Search Catalogue of Life for taxonomic IDs
#'
#' @export
#' @param name The string to search for. Only exact matches found the name given
#'   	will be returned, unless one or wildcards are included in the search
#'   	string. An * (asterisk) character denotes a wildcard; a % (percentage)
#'    character may also be used. The name must be at least 3 characters long,
#'    not counting wildcard characters.
#' @param id The record ID of the specific record to return (only for scientific
#' 		names of species or infraspecific taxa)
#' @param start The first record to return. If omitted, the results are returned
#' 		from the first record (start=0). This is useful if the total number of
#' 		results is larger than the maximum number of results returned by a single
#' 		Web service query (currently the maximum number of results returned by a
#' 		single query is 500 for terse queries and 50 for full queries).
#' @param checklist The year of the checklist to query, if you want a specific
#' 		year's checklist instead of the lastest as default (numeric).
#' @param response (character) one of "terse" or "full"
#' @param ... Curl options passed on to \code{\link[crul]{HttpClient}}
#' @details You must provide one of name or id. The other parameters (format
#' 		and start) are optional.
#' @references \url{http://webservice.catalogueoflife.org/}
#' @return A list of data.frame's.
#' @examples \dontrun{
#' # A basic example
#' col_search(name="Apis")
#' col_search(name="Agapostemon")
#' col_search(name="Poa")
#'
#' # Get full response, i.e., more data
#' col_search(name="Apis", response="full")
#' col_search(name="Poa", response="full")
#'
#' # Many names
#' col_search(name=c("Apis","Puma concolor"))
#' col_search(name=c("Apis","Puma concolor"), response = "full")
#'
#' # An example where there is no data
#' col_search(id = "36c623ad9e3da39c2e978fa3576ad415")
#' col_search(id = "36c623ad9e3da39c2e978fa3576ad415", response = "full")
#' col_search(id = "787ce23969f5188c2467126d9a545be1")
#' col_search(id = "787ce23969f5188c2467126d9a545be1", response = "full")
#' col_search(id = c("36c623ad9e3da39c2e978fa3576ad415", 
#'   "787ce23969f5188c2467126d9a545be1"))
#' ## a synonym
#' col_search(id = "f726bdaa5924cabf8581f99889de51fc")
#' col_search(id = "f726bdaa5924cabf8581f99889de51fc", response = "full")
#' }

col_search <- function(name=NULL, id=NULL, start=NULL, checklist=NULL, 
  response="terse", ...) {

  response <- match.arg(response, c("terse", "full"))
  func <- function(x, y, ...) {
    url <- make_url(checklist)
    args <- tc(list(name = x, id = y, start = start, response = response,
                    format = "json"))
    cli <- crul::HttpClient$new(url = url, opts = list(...))
    out <- cli$get(query = argsnull(args))
    if (out$status_code >= 300) {
      warning("COL taxon not found", call. = FALSE)
    } else {
      tt <- jsonlite::fromJSON(out$parse("UTF-8"), FALSE)
      switch(
        response,
        terse = parse_terse(tt),
        full = parse_full(tt)
      )
    }
  }
  safe_func <- plyr::failwith(NULL, func)
  if (is.null(id)) {
    stats::setNames(lapply(name, safe_func, y = NULL, ...), name)
  } else {
    stats::setNames(lapply(id, safe_func, x = NULL, ...), id)
  }
}

make_url <- function(checklist) {
  if (is.null(checklist)) {
    col_base()
  } else {
    cc <- match.arg(as.character(checklist), choices = 2015:2007)
    sprintf("http://catalogueoflife.org/annual-checklist/%s/webservice", cc)
  }
}

col_base <- function() "http://www.catalogueoflife.org/col/webservice"

parse_terse <- function(x) {
  nodes <- x$results
  ldply(nodes, parsecoldata)
}

parsecoldata <- function(x){
  vals <- x[c('id', 'name', 'rank', 'name_status', 'source_database')]
  vals[sapply(vals, is.null)] <- NA
  names(vals) <- c('id', 'name', 'rank', 'name_status', 'source_database')
  bb <- data.frame(vals, stringsAsFactors = FALSE)
  names(bb)[4:5] <- c('status', 'source')
  bb$rank <- tolower(bb$rank)
  acc <- x$accepted_name
  if (is.null(acc)) {
    accdf <- data.frame(acc_id=NA, acc_name=NA, acc_rank=NA, acc_status=NA, acc_source=NA, stringsAsFactors = FALSE)
  } else {
    accdf <- data.frame(acc[c('id','name','rank','name_status','source_database')], stringsAsFactors=FALSE)
    names(accdf) <- c('acc_id','acc_name','acc_rank','acc_status','acc_source')
    accdf$acc_rank <- tolower(accdf$acc_rank)
  }
  cbind(bb, accdf)
}

parse_full <- function(x) {
  tmp <- Filter(length, x$results)
  taxize_ldfast(
    lapply(tmp, function(z) {
      switch(z$name_status,
             `accepted name` = {
               if (length(z$classification) == 0) {
                 h <- parse_one(z)
                 rank <- z$rank
                 id <- z$id
               } else {
                 h <- parse_one(z)
                 h_vals <- pluck(z$classification, "name", "")
                 h_nms <- pluck(z$classification, "rank", "")
                 class <- setNames(rbind.data.frame(h_vals), tolower(h_nms))
                 h <- cbind(h, class)
                 rank <- z$rank
                 id <- z$id
               }
             },
             `provisionally accepted name` = {
               if (length(z$classification) == 0) {
                 h <- parse_one(z)
                 rank <- z$rank
                 id <- z$id
               } else {
                 h <- parse_one(z)
                 h_vals <- pluck(z$classification, "name", "")
                 h_nms <- pluck(z$classification, "rank", "")
                 class <- setNames(rbind.data.frame(h_vals), tolower(h_nms))
                 h <- cbind(h, class)
                 rank <- z$rank
                 id <- z$id
               }
             },
             `common name` = {
               h_vals <- pluck(z$accepted_name$classification, "name", "")
               h_nms <- pluck(z$accepted_name$classification, "rank", "")
               h <- setNames(rbind.data.frame(h_vals), tolower(h_nms))
               rank <- z$accepted_name$rank
               id <- z$accepted_name$id
             },
             `synonym` = {
               h <- parse_one(z)
               name <- z$accepted_name$name
               rank <- z$accepted_name$rank
               id <- z$accepted_name$id
               name_status <- z$accepted_name$name_status
               h <- cbind(h, setNames(data.frame(id, name, rank, name_status, stringsAsFactors = FALSE),
                                      c('acc_id','acc_name','acc_rank','acc_status')))
             },
             `ambiguous synonym` = {
               h <- parse_one(z)
               name <- z$accepted_name$name
               rank <- z$accepted_name$rank
               id <- z$accepted_name$id
               name_status <- z$accepted_name$name_status
               h <- cbind(h, setNames(data.frame(id, name, rank, name_status, stringsAsFactors = FALSE),
                                      c('acc_id','acc_name','acc_rank','acc_status')))
             },
             `misapplied name` = {
               h <- parse_one(z)
               name <- z$accepted_name$name
               rank <- z$accepted_name$rank
               id <- z$accepted_name$id
               name_status <- z$accepted_name$name_status
               h <- cbind(h, setNames(data.frame(id, name, rank, name_status, stringsAsFactors = FALSE),
                                      c('acc_id','acc_name','acc_rank','acc_status')))
             }
      )
      target <- setNames(rbind.data.frame(
        c(z$name,
          rank,
          id,
          z$name_status)),
        c("name", "rank", "id", "name_status"))
      target$rank <- tolower(target$rank)
      tempdf <- cbind(target, h)
      tempdf[] <- lapply(tempdf, as.character)
      tempdf
    })
  )
}

parse_one <- function(z) {
  scrut <- z$record_scrutiny_date
  scrutie <- if (is.null(scrut[[1]]) || scrut[[1]] == FALSE) FALSE else TRUE
  if (scrutie) scrut <- data.frame(record_scrutiny_date = scrut$scrutiny, stringsAsFactors = FALSE)
  refs <- z$references
  refsie <- if (is.null(refs) || length(refs) == 0) FALSE else TRUE
  if (refsie) refs <- data.frame(tc(refs[[1]]), stringsAsFactors = FALSE)
  lst <- pop(z, c("distribution", "classification", "synonyms", "common_names",
                  "record_scrutiny_date", "references", "accepted_name", "child_taxa",
                  "name_html"))
  df <- data.frame(lst, stringsAsFactors = FALSE)
  if (is(scrut, "data.frame")) df <- cbind(df, scrut)
  if (is(refs, "data.frame")) df <- cbind(df, refs)
  df
}
