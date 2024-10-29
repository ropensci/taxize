#' @title Resolve names using Global Names Recognition and Discovery.
#'
#' @description Uses the Global Names Recognition and Discovery service, see
#'   http://gnrd.globalnames.org/
#'
#'   Note: this function sometimes gives data back and sometimes not. The API
#'   that this function is extremely buggy.
#'
#' @export
#' @param url (character) If text parameter is empty, and url is given, GNfinder will
#'   process the URL and will find names in the content of its body.
#' @param text (character) Contains the text which will be checked for scientific names. If
#'   this parameter is not empty, the url parameter is ignored.
#' @param format (character) Sets the output format. It can be set to: "csv" (the default),
#'   "tsv", or "json".
#' @param bytes_offset (logical) This changes how the position of a detected name in text
#'   is calculated. Normally a name's start and end positions are given as the
#'   number of UTF-8 characters from the beginning of the text. If bytesOffset
#'   flag is true, the start and end offsets are recalculated in the number of
#'   bytes.
#' @param return_content (logical) If this is `TRUE`, the text used for the name
#'   detection is returned back. This is especially useful if the input was not
#'   a plain UTF-8 text and had to be prepared for name-finding. Then the
#'   returned content can be used together with start and end fields of detected
#'   name-strings to locate the strings in the text.
#' @param unique_names (logical) If this is `TRUE`, the output returns a list of
#'   unique names, instead of a list of all name occurrences. Unique list of
#'   names does not provide position information of a name in the text.
#' @param ambiguousNames (logical) If this is `TRUE`, strings which are simultaneously
#'   scientific names and "normal" words are not filtered out from the results.
#'   For example generic names like America, Cancer, Cafeteria will be returned
#'   in the results.
#' @param no_bayes (logical) If this is `TRUE`, only heuristic algorithms are used for
#'   name detection.
#' @param odds_details (logical) If true, the result will contain odds of all features
#'   used for calculation of NaiveBayes odds. Odds describe probability of a
#'   name to be 'real'. The higher the odds, the higher the probability that a
#'   detected name is not a false positive. Odds are calculated by
#'   multiplication of the odds of separate features. Odds details explain how
#'   the final odds value is calculated.
#' @param language (character) The language of the text. Language value is used for
#'   calculation of Bayesian odds. If this parameter is not given, eng is used
#'   by default. Currently only English and German languages are supported.
#'   Valid values are: `eng`, `deu`, `detect`.
#' @param words_around (integer) Allows to see the context surrounding a name-string. This
#'   sets the number of words located immediately before or after a detected
#'   name. These words are then returned in the output. Default is 0, maximum
#'   value is 5.
#' @param verification (character) When this `TRUE`, there is an additional
#'   verification step for detected names. This step requires internet
#'   connection and uses https://verifier.globalnames.org/api/v1 for
#'   verification queries.
#' @param sources Pipe separated list of data source ids to resolve found names
#'   against. See list of Data Sources
#'   http://resolver.globalnames.org/data_sources
#' @param ... Further args passed to [crul::verb-GET]
#' @param detect_language Defunct. See the `language` option.
#' @param all_data_sources Defunct. The API used no longer supports this option.
#' @param data_source_ids Defunct. See the `sources` option.
#' @param file Defunct. If you feel this is important functionality submit an
#'   issue at "https://github.com/ropensci/taxize"
#' @param unique Defunct. See the `unique_names` option.
#' @param engine Defunct. The API used no longer supports this option.
#' @param verbatim Defunct. The API used no longer supports this option.
#' @author Scott Chamberlain
#' @return A list of length two, first is metadata, second is the data as a
#'   data.frame.
#' @details One of url, file, or text must be specified - and only one of them.
#' @examples \dontrun{
#' # Get data from a website using its URL
#' scrapenames('https://en.wikipedia.org/wiki/Spider')
#' scrapenames('https://en.wikipedia.org/wiki/Animal')
#' scrapenames('https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0095068')
#' scrapenames('https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0080498')
#' scrapenames('http://ucjeps.berkeley.edu/cgi-bin/get_JM_treatment.pl?CARYOPHYLLACEAE')
#'
#' # Scrape names from a pdf at a URL
#' url <- 'https://journals.plos.org/plosone/article/file?id=
#' 10.1371/journal.pone.0058268&type=printable'
#' scrapenames(url = sub('\n', '', url))
#'
#' # With arguments
#' scrapenames(url = 'https://www.mapress.com/zootaxa/2012/f/z03372p265f.pdf',
#'   unique_names=TRUE)
#' scrapenames(url = 'https://en.wikipedia.org/wiki/Spider',
#'   data_source_ids=c(1, 169))
#'
#' # Get data from a file
#' speciesfile <- system.file("examples", "species.txt", package = "taxize")
#' scrapenames(file = speciesfile)
#'
#' nms <- paste0(names_list("species"), collapse="\n")
#' file <- tempfile(fileext = ".txt")
#' writeLines(nms, file)
#' scrapenames(file = file)
#'
#' # Get data from text string
#' scrapenames(text='A spider named Pardosa moesta Banks, 1892')
#'
#' # return OCR content
#' scrapenames(url='https://www.mapress.com/zootaxa/2012/f/z03372p265f.pdf',
#'   return_content = TRUE)
#' }
scrapenames <- function(
    url = NULL,
    text = NULL,
    format = 'csv',
    bytes_offset = FALSE,
    return_content = FALSE,
    unique_names = TRUE,
    ambiguous_names = FALSE,
    no_bayes = FALSE,
    odds_details = FALSE,
    language = 'detect',
    words_around = 0,
    verification = TRUE,
    sources = NULL,
    all_matches = FALSE,
     ...,
    file = NULL,
    unique = NULL,
    engine = NULL,
    detect_language = NULL,
    data_source_ids = NULL,
    method = NULL
) {
  
  # Error if defunct parameters are used.
  if (!is.null(unique)) {
    stop(call. = FALSE, 'The `unique` option is defunct. See the `unique_names` option. ')
  }
  if (!is.null(engine)) {
    stop(call. = FALSE, 'The `engine` option is defunct. The API no longer supports this option. ')
  }
  if (!is.null(detect_language)) {
    stop(call. = FALSE, 'The `detect_language` option is defunct. See the `language` option. ')
  }
  if (!is.null(data_source_ids)) {
    stop(call. = FALSE, 'The `data_source_ids` option is defunct. See the `source` option. ')
  }
  if (!is.null(method)) {
    stop(call. = FALSE, 'This function can no longer submit files. If you feel this is important functionality submit an issue at "https://github.com/ropensci/taxize".')
  }
  
  # Validate parameters
  if (! format %in% c('csv', 'tsv', 'json')) {
    stop(call. = FALSE, 'The `format` option must be "csv", "tsv", or "json". "', format, '" was the value given')
  }
  
  # Make query
  base <- "http://gnrd.globalnames.org/api/v1/find"
  args <- list(
    text = text,
    url = url,
    format = format,
    bytesOffset = bytes_offset,
    returnContent = return_content,
    uniqueNames = unique_names,
    ambiguousNames = ambiguous_names,
    noBayes = no_bayes,
    oddsDetails = odds_details,
    language = language,
    wordsAround = words_around,
    verification = verification,
    sources = sources,
    allMatches = all_matches
  )
  cli <- crul::HttpClient$new(base, headers = tx_ual, opts = list(...))
  response <- cli$post(body = args, encode = "multipart")
  
  # Parse and return results
  switch (format,
    csv = read.csv(text = response$parse("UTF-8")),
    tsv = read.csv(text = response$parse("UTF-8"), sep = '\t'),
    json = jsonlite::fromJSON(response$parse("UTF-8")),
    other = stop("Invalid 'format' option.")
  )
}

