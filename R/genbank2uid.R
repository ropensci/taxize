#' Get NCBI taxonomy UID from GenBankID
#'
#' @export
#' @param id A GenBank accession alphanumeric string, or a gi numeric string.
#' @param batch_size The number of queries to submit at a time.
#' @param key (character) NCBI Entrez API key. optional. See Details.
#' @param ... Curl args passed on to \code{\link[crul]{HttpClient}}
#' @details See \url{http://www.ncbi.nlm.nih.gov/Sitemap/sequenceIDs.html} for
#' help on why there are two identifiers, and the difference between them.
#' 
#' @section Authentication:
#' See \code{\link{taxize-authentication}} for help on authentication
#'
#' @return one or more NCBI taxonomic IDs
#' @examples \dontrun{
#' # with accession numbers
#' genbank2uid(id = 'AJ748748')
#' genbank2uid(id = 'Y13155')
#' genbank2uid(id = 'X78312')
#' genbank2uid(id = 'KM495596')
#'
#' # with gi numbers
#' genbank2uid(id = 62689767)
#' genbank2uid(id = 22775511)
#' genbank2uid(id = 156446673)
#'
#' # pass in many accession or gi numbers
#' genbank2uid(c(62689767,156446673))
#' genbank2uid(c('X78312','KM495596'))
#' genbank2uid(list('X78312',156446673))
#'
#' # curl options
#' res <- genbank2uid(id = 156446673, verbose = TRUE)
#' }
genbank2uid <- function(id, batch_size = 100, key = NULL, ...) {
  assert(batch_size, c("integer", "numeric"))
  process_batch <- function(id, key, ...) {
    #removes version number of accession ids
    id <- gsub(pattern = "\\.[0-9]+$", "", id)
    
    key <- getkey(key, "ENTREZ_KEY")
    
    # Make NCBI eutils query
    query <- tc(list(db = "nucleotide", id = paste(id, collapse = ",")))
    if (!is.null(key) && nzchar(key)) {
      query <- c(query, list(api_key = key))
    }
    
    # Execute query
    cli <- crul::HttpClient$new(url = ncbi_base(), opts = list(...))
    res <- cli$get("entrez/eutils/esummary.fcgi", query = query)
    res$raise_for_status()
    parsed_xml <- read_xml(res$parse('UTF-8'))
    
    # Extract taxon ID and sequences name
    taxon_ids <- xml_text(
      xml_find_all(parsed_xml,
        "//eSummaryResult//DocSum//Item[@Name='TaxId']"))
    titles <- xml_text(
      xml_find_all(parsed_xml,
        "//eSummaryResult//DocSum//Item[@Name='Title']"))
    
    # Add NAs for failed queries
    raw_errors <- xml_text(xml_find_all(parsed_xml, "//eSummaryResult//ERROR"))
    if (length(raw_errors) > 0) {
      error_regexes <- c('^Invalid uid (.+) at position=[0-9]+$', 
                         '^Failed uid="(.+)"$')
      error_ids <- lapply(error_regexes,
                          function(r) stringr::str_match(raw_errors, r)[,2])
      error_ids <- unlist(error_ids)
      error_ids <- error_ids[!is.na(error_ids)]
      error_ids <- unique(error_ids)
      add_error_na <- function(values) {
        output <- rep(NA, length(id))
        output[- match(error_ids, id)] <- values
        return(output)
      }
      taxon_ids <- add_error_na(taxon_ids)
      titles <- add_error_na(titles)
    }
    
    # combine results into a data.frame and return
    result <- data.frame(id = taxon_ids, name = titles,
                         stringsAsFactors = FALSE)
    # NCBI limits requests to three per second when no key
    if (is.null(key) || !nzchar(key)) Sys.sleep(0.33)
    return(result)
  }
  # Run each batch and combine
  batches <- split(id, ceiling(seq_along(id) / batch_size))
  batch_results <- lapply(batches,
                          function(x) map_unique(x, process_batch, 
                            key = key, ...))
  result <- do.call(rbind, batch_results)
  
  # Convert to list format
  output <- lapply(seq_len(nrow(result)), function(i) {
    my_uid <- result[i, "id"]
    my_uid <- as.uid(result[i, "id"], check = FALSE)
    if (is.na(my_uid)) {
      attr(my_uid, "match") <- "not found"
      attr(my_uid, "name") <- "unknown"
    } else {
      attr(my_uid, "match") <- "found"
      attr(my_uid, "name") <- result[i, "name"]
    }
    return(my_uid)
  })
  
  # Alert user to errors
  if (any(is.na(result$id))) {
    failed <- paste0("[", which(is.na(result$id)), "] ", id[is.na(result$id)])
    warning(paste0("The following ", sum(is.na(result$id)), " of ", nrow(result),
                   " queries could not be found:\n  ",
                   limited_print(failed, type = "silent")), call. = FALSE)
   }
  return(output)
}

is_acc <- function(x){
  gg <- suppressWarnings(as.numeric(x))
  is.na(gg)
}

#=====================================================================
# get indexes of a unique set of the input
unique_mapping <- function(input) {
  unique_input <- unique(input)
  vapply(input, function(x) which(x == unique_input), numeric(1))
}

#=====================================================================
# run a function on unique values of a iterable
map_unique <- function(input, func, ...) {
  input_class <- class(input)
  unique_input <- unique(input)
  class(unique_input) <- input_class
  func(unique_input, ...)
  # func(unique_input, ...)[unique_mapping(input)]
}
