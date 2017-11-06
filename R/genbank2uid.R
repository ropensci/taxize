#' Get NCBI taxonomy UID from GenBankID
#'
#' @export
#' @param id A GenBank accession alphanumeric string, or a gi numeric string.
#' @param batch_size The number of queries to submit at a time.
#' @param ... Curl args passed on to \code{\link[httr]{GET}}
#' @details See \url{http://www.ncbi.nlm.nih.gov/Sitemap/sequenceIDs.html} for
#' help on why there are two identifiers, and the difference between them.
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
#' library('httr')
#' genbank2uid(id = 156446673, config=verbose())
#' }
genbank2uid <- function(id, batch_size = 100, ...) {
  assert(batch_size, c("integer", "numeric"))
  process_batch <- function(id, ...) {
    #removes version number of accession ids
    id <- gsub(pattern = "\\.[0-9]+$", "", id)
    
    # Make NCBI eutils query
    query <- paste0(
      ncbi_base(),
      "/entrez/eutils/esummary.fcgi?db=nucleotide&id=",
      paste(id, collapse = ",")
    )
    
    # Execute query
    xml_result <- GET(query, ...)
    stop_for_status(xml_result)
    parsed_xml <- read_xml(con_utf8(xml_result))
    
    # Extract taxon ID and sequences name
    taxon_ids <- xml_text(xml_find_all(parsed_xml,
                                       "//eSummaryResult//DocSum//Item[@Name='TaxId']"))
    titles <- xml_text(xml_find_all(parsed_xml,
                                    "//eSummaryResult//DocSum//Item[@Name='Title']"))
    
    # Add NAs for failed queries
    raw_errors <- xml_text(xml_find_all(parsed_xml, "//eSummaryResult//ERROR"))
    if (length(raw_errors) > 0) {
      error_pos <- gsub(raw_errors, pattern = "", replacement = "")
      error_ids <- stringr::str_match(raw_errors,
                                      '^Invalid uid (.+) at position=[0-9]+$')[,2]
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
    Sys.sleep(0.34) # NCBI limits requests to three per second
    return(result)
  }
  # Run each batch and combine
  batches <- split(id, ceiling(seq_along(id) / batch_size))
  batch_results <- lapply(batches,
                          function(x) map_unique(x, process_batch, ...))
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
