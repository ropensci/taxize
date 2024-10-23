#' @title Resolve names using Global Names Recognition and Discovery.
#'
#' @description Uses the Global Names Recognition and Discovery service, see
#'   http://gnrd.globalnames.org/
#'
#'   Note: this function sometimes gives data back and sometimes not. The API
#'   that this function is extremely buggy.
#'
#' @export
#' @param url Defunct. Use the `text` input for URLs as well as text strings.
#' @param file When using multipart/form-data as the content-type, a file may be
#'   sent. This should be a path to your file on your machine.
#' @param text A text (or URL pointing to a text) for name detection.
#' @param engine (optional) (integer) Defunct. The API used no longer supports
#'   this option.
#' @param unique Defunct. See the `unique_names` option.
#' @param unique_names (optional) (logical) If `TRUE` (the default), the output
#'   returns unique names, instead of all name occurrences, without position
#'   information of a name in the text.
#' @param verbatim (optional) Defunct. The API used no longer supports this
#'   option.
#' @param detect_language (optional) Defunct. See the `language` option.
#' @param language The language of the text. Language value is used for
#'   calculation of Bayesian odds. If this parameter is not given, eng is used
#'   by default. Currently only English and German languages are supported.
#'   Valid values are: `eng`, `deu`, `detect`.
#' @param all_data_sources (optional) Defunct. The API used no longer supports
#'   this option.
#' @param data_source_ids (optional) Defunct. See the `sources` option.
#' @param sources Pipe separated list of data source ids to resolve found names
#'   against. See list of Data Sources
#'   http://resolver.globalnames.org/data_sources
#' @param return_content (logical) return OCR'ed text. returns text string in
#'   `x$meta$content` slot. Default: `FALSE`
#' @param ... Further args passed to [crul::verb-GET]
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
    file = NULL,
    text = NULL,
    engine = NULL,
    unique = NULL,
    unique_names = NULL,
    verbatim = NULL,
    detect_language = NULL,
    language = NULL,
    all_data_sources = NULL,
    data_source_ids = NULL,
    sources = NULL,
    return_content = FALSE, 
    ...
) {
  
  # Error if defunct parameters are used.
  if (!is.null(url)) {
    stop(call. = FALSE, 'The `url` option is defunct. Use the `text` option for URLs as well as text strings.')
  }
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
  
  method <- tc(list(url = url, file = file, text = text))
  if (length(method) > 1) {
    stop("Only one of url, file, or text can be used", call. = FALSE)
  }

  base <- "http://gnrd.globalnames.org/api/v1/find"
  if (!is.null(data_source_ids))
    data_source_ids <- paste0(data_source_ids, collapse = "|")
  args <- tc(list(
    text = text,
    unique_names = unique_names,
    verbatim = verbatim,
    language = language,
    source = source,
    return_content = as_l(return_content)
  ))
  cli <- crul::HttpClient$new(base, headers = tx_ual, opts = list(...))
  if (names(method) == 'url') {
    tt <- cli$get(query = args)
    tt$raise_for_status()
    out <- jsonlite::fromJSON(tt$parse("UTF-8"))
    token_url <- out$token_url
  } else {
    if (names(method) == "text") {
      tt <- cli$post(body = list(text = text), encode = "form",
                 followlocation = 0)
    } else {
      tt <- cli$post(query = argsnull(args), encode = "multipart",
                 body = list(file = crul::upload(file)), 
                 followlocation = 0)
    }
    if (tt$status_code != 303) tt$raise_for_status()
    token_url <- tt$response_headers$location
    }

  st <- 303
  while (st == 303) {
    dat <- crul::HttpClient$new(token_url, headers = tx_ual)$get()
    dat$raise_for_status()
    datout <- jsonlite::fromJSON(dat$parse("UTF-8"))
    st <- datout$status
  }
  meta <- datout[!names(datout) %in% c("names")]
  list(meta = meta, data = nmslwr(datout$names))
}

