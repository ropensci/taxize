#' Get metadata about GNA data sources
#'
#' Downloads metadata about Global Names Architecture (GNA) data sources
#' available to be used in other GNA functions.
#'
#' @param output_type What format of output to return. Either `'json'`,
#'   `'list'`, or `'table'`.
#' @param ... Passed to [crul::HttpClient].
#'
#' @author Zachary S.L. Foster
#'
#' @examples \dontrun{
#' 
#' gna_data_sources()
#' }
#' 
#' @export
gna_data_sources <- function(output_type = 'table', ...) {
  gna_data_sources_url <- 'https://verifier.globalnames.org/api/v1/data_sources'
  
  # Check arguments
  possible_output_types <- c('json', 'table', 'list')
  if (! output_type %in% possible_output_types) {
    stop(call. = FALSE, 'The `output_type` value must be one of: ', paste0(possible_output_types, collapse = ', '))
  }
  
  # Make and parse API call
  api <- crul::HttpClient$new(gna_data_sources_url, headers = tx_ual, opts = list(...))
  response <- api$get()
  response$raise_for_status()
  response_json <- response$parse("UTF-8")
  if (output_type == 'json') {
    return(response_json)
  }
  response_data <- jsonlite::fromJSON(response_json, FALSE)
  
  # Reformat response data to a list
  if (output_type == 'list') {
    return(response_data)
  }
  
  # Reformat response data to a table
  if (output_type == 'table') {
    used_cols <- c(
      'id',
      'titleShort',
      'title',
      'description',
      'curation',
      'hasTaxonData',
      'recordCount',
      'updatedAt',
      'homeURL',
      'uuid',
      'version',
      'isOutlinkReady',
      'doi'
    )
    response_table <- do.call(rbind, lapply(response_data, function(x) {
      out <- x[used_cols]
      names(out) <- used_cols
      out[unlist(lapply(out, is.null))]  <- NA
      as.data.frame(out)
    }))
    response_table <- tibble::as_tibble(response_table)
  }
  return(response_table)
}