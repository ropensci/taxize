#' Function to get API key.
#'
#' Checks first to get key from your .Rprofile or .Renviron (or similar) file
#'
#' @export
#' @param x (character) An API key, defaults to `NULL`
#' @param service (character) The API data provider, used to match to
#' default guest key (for Tropicos and EOL; there's no guest
#' key for NCBI or IUCN, for which you have to get your own)
#' @examples \dontrun{
#' getkey(service="tropicos")
#' getkey(service="eol")
#' getkey(service="iucn")
#' getkey(service="entrez")
#' }
getkey <- function(x = NULL, service) {
  if (is.null(x)) {
    keynames <- c("TROPICOS_KEY", "EOL_KEY", "IUCN_REDLIST_KEY", "ENTREZ_KEY")
    service <- grep(service, keynames, ignore.case = TRUE, value = TRUE)
    key <- getOption(service)
    if (is.null(key)) key <- Sys.getenv(service, "")

    # if Entrez, return either way as no key actually required
    if (service == "ENTREZ_KEY") {
      if (is.null(key) || !nzchar(key)) {
        message("No ENTREZ API key provided\n Get one via taxize::use_entrez()\nSee https://ncbiinsights.ncbi.nlm.nih.gov/2017/11/02/new-api-keys-for-the-e-utilities/")
        return(NULL)
      } else {
        return(key)
      }
    }

    # if IUCN, stop if no key as a key is required
    if (service == "IUCN_REDLIST_KEY") {
      if (is.null(key) || !nzchar(key)) {
        stop("No IUCN API key provided\nSee taxize::use_iucn()")
      }
      return(key)
    }

    if (is.null(key) || !nzchar(key)) {
      keys <- c("00ca3d6a-cbcc-4924-b882-c26b16d54446",
                "44f1a53227f1c0b6238a997fcfe7513415f948d2")
      names(keys) <- keynames[1:2]
      key <- keys[[service]]
      key_helpers <- c("taxize::use_tropicos()",
                       "taxize::use_eol()")
      names(key_helpers) <- keynames[1:2]
      message(paste("Using default key: Please get your own API key via ",
                    key_helpers[service], sep = ""))
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
