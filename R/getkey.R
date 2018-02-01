#' Function to get API key.
#'
#' Checks first to get key from your .Rprofile or .Renviron (or similar) file. 
#' See Details.
#'
#' @export
#' @aliases taxize-authentication
#' @param x (character) An API key, defaults to \code{NULL}
#' @param service (character) The API data provider, used to match to 
#' default guest key (for Tropicos, EOL and Plantminer; there's no guest
#' key for NCBI, you have to get your own)
#' 
#' @details
#' Save your API keys with the following names:
#' \itemize{
#'  \item Tropicos: R option or env var as 'TROPICOS_KEY'
#'  \item EOL: R option or env var as 'EOL_KEY'
#'  \item Plantminer: R option or env var as 'PLANTMINER_KEY'
#'  \item ENTREZ: R option or env var as 'ENTREZ_KEY'
#' }
#' 
#' See \code{\link{Startup}} for help on R options and environment 
#' variables
#' 
#' Note that NCBI Entrez doesn't require that you use an API key, 
#' but you should get higher rate limit with a key.
#' 
#' @section EOL:
#' EOL requires an API key. You can pass in your EOL api
#' key in the function call like
#' \code{sci2comm('Helianthus annuus', key="<your eol api key>")}. You can
#' also store your EOL API key in your .Rprofile file as
#' \code{options(EOL_KEY = "<your eol api key>")}, or just for the current
#' session by running \code{options(EOL_KEY = "<your eol api key>")} in
#' the console.
#'
#' @section IUCN:
#' IUCN requires an API key. See \code{\link[rredlist]{rredlist-package}} 
#' for help on authentiating with IUCN Redlist
#' 
#' @section NCBI Entrez:
#' From NCBI's docs: "E-utils users are allowed 3 requests/second without an 
#' API key. Create an API key (in your account at 
#' https://www.ncbi.nlm.nih.gov/account/) to increase your e-utils limit to 10 
#' requests/second. Contact our help department (eutilities@ncbi.nlm.nih.gov) 
#' if you need higher throughput. Only one API Key per user. Replacing or 
#' deleting will inactivate the current key. Refer to documentation 
#' (https://www.ncbi.nlm.nih.gov/books/NBK25497/) for more."
#' 
#' @examples \dontrun{
#' getkey(service="tropicos")
#' getkey(service="eol")
#' getkey(service="plantminer")
#' getkey(service="entrez")
#' }
getkey <- function(x = NULL, service) {
  if (is.null(x)) {
    keynames <- c("TROPICOS_KEY", "EOL_KEY", "PLANTMINER_KEY", "ENTREZ_KEY")
    service <- grep(service, keynames, ignore.case = TRUE, value = TRUE)
    key <- getOption(service)
    if (is.null(key)) key <- Sys.getenv(service, "")

    # if Entrez, return either way as no key actually required
    if (service == "ENTREZ_KEY") {
      if (is.null(key) || !nzchar(key)) {
        message("No ENTREZ API key provided\nSee https://ncbiinsights.ncbi.nlm.nih.gov/2017/11/02/new-api-keys-for-the-e-utilities/")
      }
      return(key)
    }

    if (is.null(key) || !nzchar(key)) {
      keys <- c("00ca3d6a-cbcc-4924-b882-c26b16d54446",
                "44f1a53227f1c0b6238a997fcfe7513415f948d2",
                "744343442")
      names(keys) <- keynames[1:3]
      key <- keys[[service]]
      urls <- c("http://services.tropicos.org/help?requestkey",
                "http://eol.org/users/register",
                "http://www.plantminer.com/")
      names(urls) <- keynames[1:3]
      message(paste("Using default key: Please get your own API key at ",
                    urls[service], sep = ""))
    } else if (inherits(key, "character")) {
      key <- key
    } else {
      stop("check your key - it should be a character string", 
        call. = FALSE)
    }
  } else {
    key <- x
  }

  return(key)
}
