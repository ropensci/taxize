#' Verify a list of scientific names against biodiversity data-sources.
#'
#' This service parses incoming names, executes exact or fuzzy matching as
#' required, and returns the best-scored result. Optionally, it can also return
#' matches from data-sources selected by a user.
#'
#' @export
#'
#' @param names A `character` vector of taxon names to verify.
#' @param data_sources A `character` or `integer` vector with numbers
#'   corresponding to data sources. See the Global Names Architecture
#'   documentation for a list of available options.
#' @param all_matches When `TRUE`, return all found matches, not only the best
#'   one. Multiple results are returned in results. These results are sorted by
#'   matching quality, the first result is the same as bestResult.
#' @param capitalize When `TRUE`, capitalize the first letter of a name-string.
#' @param species_group When `TRUE`, expands the search to species group where
#'   applicable.
#' @param fuzzy_uninomial When `TRUE`, allows fuzzy matching for uninomial
#'   names.
#' @param stats When `TRUE`, finds out a kingdom and a taxon (main taxon) that
#'   contain most names. It only takes in account the names matched to the
#'   Catalogue of Life entries. This option is ignored, if the Catalogue of Life
#'   is not included in data-sources.
#' @param main_taxon_threshold A `numeric` vector from 0.5 to 1. This sets the
#'   minimal percentage for the main taxon discovery.
#' @param output_type A `character` vector of length 1, either `table` or
#'   `list`, indicating the format of the output. The tabular output only
#'   contains values that consistently appear in all results, so `list` output
#'   can have additional information. For `list` and `json` outputs, only values
#'   for unique taxon names are returned, but the `table` output has rows that
#'   correspond 1-1 with the input data.
#' @param ... Curl options passed on to [crul::HttpClient]
#'
#' @return Depends on the value of the `output_type` option
#'
#' @author Zachary S.L. Foster
#'
#' @examples \dontrun{
#' gna_verifier(c("Helianthus annuus", "Homo saapiens"))
#' gna_verifier(c("Helianthus annuus", "Homo saapiens"), all_matches = TRUE)
#' }
gna_verifier <- function(
    names,
    data_sources = c(1, 12),
    all_matches = FALSE,
    capitalize = FALSE,
    species_group = FALSE,
    fuzzy_uninomial = FALSE,
    stats = FALSE,
    main_taxon_threshold = 0.5,
    output_type = 'table',
    ...
) {
  batch_size <- 100 # How many names to lookup with each api call
  
  # Parse and verify input options
  data_sources <- as.character(data_sources)
  is_number <- grepl(data_sources, pattern = '[0-9]+')
  if (any(! is_number) || length(data_sources) == 0) {
    stop(call. = FALSE, 'The `data_sources` input must be a vector of numbers with at least one value.')
  }
  data_sources <- paste0(data_sources, collapse = '|')
  
  check_if_logical <- function(value, name) {
    if (length(value) != 1 || is.na(value) || ! is.logical(value)) {
      stop(call. = FALSE, 'The `', name, '` input must be a TRUE/FALSE vector of length 1.')
    }
  }
  check_if_logical(all_matches, 'all_matches')
  check_if_logical(capitalize, 'capitalize')
  check_if_logical(species_group, 'species_group')
  check_if_logical(fuzzy_uninomial, 'fuzzy_uninomial')
  check_if_logical(stats, 'stats')
  
  if (length(main_taxon_threshold) != 1 || ! is.numeric(main_taxon_threshold) || main_taxon_threshold < 0.5 || main_taxon_threshold > 1) {
    stop(call. = FALSE, 'The `main_taxon_threshold` input must be a single number between 0.5 to 1')
  }
  
  output_type <- match.arg(output_type, c('table', 'list', 'json'))
  
  # Convert input to unique values to avoid redundant API wprk
  unique_names <- unique(names)
  name_batches <- split(unique_names, ceiling(seq_along(unique_names) / batch_size)) 
  
  batch_data <- lapply(name_batches, function(batch) {
    # Format the API GET request
    base_url <- 'https://verifier.globalnames.org/'
    args <- c(
      data_sources = paste0(data_sources, collapse = '|'),
      all_matches = tolower(as.character(all_matches)),
      capitalize = tolower(as.character(capitalize)),
      species_group = tolower(as.character(species_group)),
      fuzzy_uninomial = tolower(as.character(fuzzy_uninomial)),
      stats = tolower(as.character(stats))
    )
    formatted_args <- paste0(paste0(names(args), '=', args), collapse = '&')
    formatted_path <- paste0(
      'api/v1/verifications/',
      paste0(batch, collapse = '|'),
      '?', formatted_args
    )
    
    # Make and parse API call
    api <- crul::HttpClient$new(base_url, headers = tx_ual, opts = list(...))
    response <- api$get(path = formatted_path)
    response$raise_for_status()
    response_json <- response$parse("UTF-8")
    response_data <- jsonlite::fromJSON(response_json, FALSE)
    return(response_data$names)
  })
 
  # Combine batch data to a single list
  response_data <- unlist(batch_data, recursive = FALSE)
  names(response_data) <- unique_names
  if (output_type == 'list') {
    return(response_data)
  }
  
  # Reformat response data to a table
  used_cols <- c(
    'submittedName',
    'dataSourceId',
    'dataSourceTitleShort',
    'curation',
    'recordId',
    'entryDate', 
    'sortScore',
    'matchedNameID',
    'matchedName', 
    'matchedCardinality',
    'matchedCanonicalSimple',
    'matchedCanonicalFull', 
    'currentRecordId', 
    'currentNameId',
    'currentName',
    'currentCardinality',
    'currentCanonicalSimple', 
    'currentCanonicalFull', 
    'taxonomicStatus',
    'isSynonym',
    'editDistance',
    'stemEditDistance',
    'matchType',
    'cardinalityScore', 
    'infraSpecificRankScore',
    'fuzzyLessScore',
    'curatedDataScore',
    'authorMatchScore',
    'acceptedNameScore',
    'parsingQualityScore'
  )
  convert_entry_to_row <- function(x, input_name) {
    if (is.null(x)) { # If there was no match
      output <- c(input_name, rep(NA, length(used_cols)))
      names(output) <- c('submittedName', used_cols)
      output['matchType'] <- 'NoMatch'
    } else {
      output <- c(input_name, unlist(x))
      names(output)[1] <- 'submittedName'
    }
    parsed_names <- vapply(strsplit(names(output), split = '\\.'),
                           function(y) y[length(y)], FUN.VALUE = character(1))
    names(output) <- parsed_names
    return(as.data.frame(as.list(output[used_cols])))
  }
  if (all_matches) {
    response_table <- do.call(rbind, lapply(response_data, function(x) {
      do.call(rbind, lapply(x$results, function(y) convert_entry_to_row(y, x$name)))
    }))    
  } else {
    response_table <- do.call(rbind, lapply(response_data, function(x) {
      convert_entry_to_row(x$bestResult, x$name)
    }))
  }
  
  # Reduplicate rows to match input
  split_table <- split(response_table, response_table$submittedName)
  submitted_names <- vapply(split_table, function(x) x$submittedName[1], FUN.VALUE = character(1))
  response_table <- do.call(rbind, split_table[match(names, submitted_names)])
  row.names(response_table) <- NULL
  
  # Format table to be more useful to the user
  numeric_cols <- c(
    'sortScore',
    'matchedCardinality',
    'currentCardinality',
    'editDistance',
    'stemEditDistance',
    'cardinalityScore',
    'infraSpecificRankScore',
    'fuzzyLessScore', 
    'curatedDataScore',
    'authorMatchScore',
    'acceptedNameScore', 
    'parsingQualityScore'
  ) 
  response_table[numeric_cols] <- lapply(response_table[numeric_cols], as.numeric)
  response_table$isSynonym <- as.logical(response_table$isSynonym)
  response_table <- tibble::as_tibble(response_table)
  
  return(response_table)
}