#' Search Catalogue of Life for taxonomic IDs
#'
#' @export
#' @param name The string to search for. Only exact matches found the name given
#' will be returned, unless one or wildcards are included in the search
#' string. An * (asterisk) character denotes a wildcard; a percent
#' character may also be used. The name must be at least 3 characters long,
#' not counting wildcard characters.
#' @param id The record ID of the specific record to return (only for scientific
#' names of species or infraspecific taxa)
#' @param start The first record to return. If omitted, the results are returned
#' from the first record (start=0). This is useful if the total number of
#' results is larger than the maximum number of results returned by a single
#' Web service query (currently the maximum number of results returned by a
#' single query is 500 for terse queries and 50 for full queries).
#' @param checklist The year of the checklist to query, if you want a specific
#' year's checklist instead of the lastest as default (numeric). Options include
#' 2007 to whatever the current year is. By default, the current year is used.
#' Using 2014 and older we only give back an XML object the user can parse
#' on their own
#' @param response (character) one of "terse" or "full"
#' @param ... Curl options passed on to [crul::HttpClient]
#' @details You must provide one of name or id. The other parameters (format
#' and start) are optional.
#' @references <http://webservice.catalogueoflife.org/>
#' @section Rate limiting:
#' COL introduced rate limiting recently (writing this on 2019-11-14),
#' but we've no information on what the rate limits are. If you do run into
#' this you'll see an error like "Error: Too Many Requests (HTTP 429)",
#' you'll need to time your requests to avoid the rate limiting, for
#' example, by putting `Sys.sleep()` in between simultaneous requests.
#' @return When checklist is 2015 or great, a list of data.frame's, named
#' with the input vector of name's 
#' or id's, each data.frame has attributes you can access like 
#' `attr(df, "error_message")`:
#' 
#' * id
#' * name
#' * total_number_of_results
#' * number_of_results_returned
#' * start
#' * error_message
#' * version
#' * rank
#' 
#' If checklist is 2014 or less, COL did not provide JSON as a response
#' format, so we return `xml_document` objects for each input name or id
#'
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
#' # checklist year 2014 or earlier returns an xml_document
#' col_search(name="Agapostemon", checklist=2012)
#' col_search(name=c("Agapostemon", "Megachile"), checklist=2011)
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

col_search <- function(name = NULL, id = NULL, start = NULL, checklist = NULL,
  response = "terse", ...) {

  response <- match.arg(response, c("terse", "full"))
  func <- function(x, y, ...) {
    url <- make_url(checklist)
    args <- tc(list(name = x, id = y, start = start, response = response,
                    format = "json"))
    cli <- crul::HttpClient$new(url = url, headers = tx_ual, opts = list(...))
    out <- cli$get(query = argsnull(args))
    out$raise_for_status()

    if (
      as.numeric(checklist) <= 2014 &&
      grepl("xml", out$response_headers$`content-type`)
    ) {
      txt <- out$parse("UTF-8")
      return(xml2::read_xml(txt))
    }
    tt <- tryCatch(jsonlite::fromJSON(out$parse("UTF-8"), FALSE),
      error = function(e) e)
    mssg <- ""
    if (inherits(tt, "error") || nzchar(tt$error_message)) {
      if (nzchar(tt$error_message)) {
        mssg <- tt$error_message
      } else {
        hs <- out$status_http()
        mssg <- sprintf("%s: %s", hs$status_code, hs$explanation)
      }
    } else {
      mssg <- tt$error_message
    }
    if (nzchar(mssg)) warning(mssg, call. = FALSE)
    switch(
      response,
      terse = col_meta(parse_terse(tt), tt),
      full = col_meta(parse_full(tt), tt)
    )
  }
  if (is.null(id)) {
    stats::setNames(lapply(name, func, y = NULL, ...), name)
  } else {
    stats::setNames(lapply(id, func, x = NULL, ...), id)
  }
}

make_url <- function(checklist) {
  if (is.null(checklist)) {
    col_base()
  } else {
    year_current <- as.numeric(format(Sys.Date(), "%Y")) - 1
    cc <- match.arg(as.character(checklist), choices = 2007:year_current)
    sprintf("http://catalogueoflife.org/annual-checklist/%s/webservice", cc)
  }
}

col_base <- function() "http://www.catalogueoflife.org/col/webservice"

col_meta <- function(y, x) {
  x$results <- NULL
  do.call(structure, c(list(.Data = y %||% data.frame(NULL)), x))
}

parse_terse <- function(x) {
  nodes <- x$results
  dt2df(lapply(nodes, parsecoldata), idcol = FALSE)
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
    accdf <- data.frame(acc_id=NA, acc_name=NA, acc_rank=NA,
      acc_status=NA, acc_source=NA, stringsAsFactors = FALSE)
  } else {
    accdf <- data.frame(acc[c('id','name','rank','name_status','source_database')],
      stringsAsFactors=FALSE)
    names(accdf) <- c('acc_id','acc_name','acc_rank','acc_status','acc_source')
    accdf$acc_rank <- tolower(accdf$acc_rank)
  }
  cbind(bb, accdf)
}

parse_full <- function(x) {
  tmp <- Filter(length, x$results)
  taxize_ldfast(
    lapply(tmp, function(z) {
      switch(
        z$name_status,
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
         # drop not accepted vars
         h$id <- h$name <- h$rank <- h$name_status <- NULL
         # set accepted name vars
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
          z$rank,
          z$id,
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
  if (scrutie) scrut <- data.frame(record_scrutiny_date = scrut$scrutiny,
    stringsAsFactors = FALSE)
  refs <- z$references
  refsie <- if (is.null(refs) || length(refs) == 0) FALSE else TRUE
  if (refsie) refs <- data.frame(tc(refs[[1]]), stringsAsFactors = FALSE)
  lst <- pop(z, c("id", "name", "rank", "name_status", "genus", "species",
    "subgenus", "distribution", "classification", "synonyms", "common_names",
    "record_scrutiny_date", "references", "accepted_name", "child_taxa",
    "name_html"))
  df <- data.frame(lst, stringsAsFactors = FALSE)
  if (is.data.frame(scrut)) df <- cbind(df, scrut)
  # if (is.data.frame(refs)) df <- cbind(df, refs)
  df
}
