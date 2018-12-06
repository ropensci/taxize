#' Function to get API key.
#'
#' Checks first to get key from your .Rprofile or .Renviron (or similar) file.
#' See Details.
#'
#' @export
#' @aliases taxize-authentication
#' @param x (character) An API key, defaults to \code{NULL}
#' @param service (character) The API data provider, used to match to
#' default guest key (for Tropicos and EOL; there's no guest
#' key for NCBI or IUCN, for which you have to get your own)
#'
#' @details
#' Get help for getting and saving your keys via \code{\link{key_helpers}}.

#' Save your API keys with the following names:
#' \itemize{
#'  \item Tropicos: R option or env var as 'TROPICOS_KEY'
#'  \item EOL: R option or env var as 'EOL_KEY'
#'  \item IUCN: R option or env var as 'IUCN_REDLIST_KEY'
#'  \item ENTREZ: R option or env var as 'ENTREZ_KEY'
#' }
#'  as R options in your \code{.Rprofile} file, or
#' as environment variables in either your \code{.Renviron} file or
#' \code{.bash_profile} file, or \code{.zshrc} file (if you use oh-my-zsh) or
#' similar. See \code{\link{Startup}} for help on R options and environment
#' variables.
#'
#' Remember to restart your R session (and to start a new shell window/tab
#' if you're using the shell) to take advantage of the new R options
#' or environment variables.
#'
#' We strongly recommend using environment variables over R options.
#'
#' Note that NCBI Entrez doesn't require that you use an API key,
#' but you should get higher rate limit with a key,
#' from 3 to 10 requests per second, so do get one.
#'
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
