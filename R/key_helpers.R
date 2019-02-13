use_fxns_urls <- list(
  tropics = "http://services.tropicos.org/help?requestkey",
  eol = "https://eol.org/users/sign_up",
  entrez = "https://www.ncbi.nlm.nih.gov/account/"
)

#' Key helpers
#'
#' @importFrom utils browseURL
#' @title Helpers to set up authentication for the different providers.
#'
#' @description Sets up authentication to diverse providers by providing
#' the user a detailed prompt.
#'
#' @name key_helpers
#' @seealso \code{\link{taxize-authentication}}
NULL

#' @section `use_tropicos()`:
#' Browses to Tropicos API key request URL and provides instruction on how to
#' store the key. After filling the form you will get the key soon, but
#' not immediately.
#' @export
#' @rdname key_helpers
use_tropicos <- function() {
  if (interactive()) {
    utils::browseURL(use_fxns_urls$tropicos)
  }

  message(paste0(
    "After getting your key set it as TROPICOS_KEY in .Renviron.\n ",
    "TROPICOS_KEY='youractualkeynotthisstring'\n ",
    "For that, use usethis::edit_r_environ()"))

  invisible(use_fxns_urls$tropicos)
}

#' @section `use_eol()`:
#' Browse EOL to help make an API key request and provides instruction on how
#' to store the key. There's no direct URL to request a key, one first needs
#' to log in or register and then to generate a key from one's Preferences
#' page.
#'
#' @export
#' @rdname key_helpers
use_eol <- function() {
  if (interactive()) {
    utils::browseURL(use_fxns_urls$eol)
  }

  message(paste0(
    "Generate your key in your (brand-new) account's Preferences page.\n ",
    "After generating your key set it as EOL_KEY in .Renviron.\n ",
    "EOL_KEY='youractualkeynotthisstring'\n ",
    "For that, use usethis::edit_r_environ()"))

  invisible(use_fxns_urls$eol)
}

#' @section `use_entrez()`:
#' Browse NCBI Entrez to help make an API key request and provides instruction
#' on how to store the key. There's no direct URL to request a key, one first
#' needs to log in or register and then to generate a key from one's account.
#'
#' Note that NCBI Entrez doesn't require that you use an API key,
#' but you should get higher rate limit with a key, so do get one.
#'
#' @export
#' @rdname key_helpers
use_entrez <- function() {
  if (interactive()) {
    utils::browseURL(use_fxns_urls$entrez)
  }

  message(paste0(
    "Create your key from your (brand-new) account's. \n ",
    "After generating your key set it as ENTREZ_KEY in .Renviron.\n ",
    "ENTREZ_KEY='youractualkeynotthisstring'\n ",
    "For that, use usethis::edit_r_environ()"))

  invisible(use_fxns_urls$entrez)
}

#' @section `use_iucn()`:
#' Browse IUCN Red List API key request URL and provides instruction on how
#' to store the key. This function wraps \code{\link[rredlist]{rl_use_iucn}}
#' from the \code{rredlist} package. After filling the form you will get
#' the key soon, but not immediately.
#' @export
#' @rdname key_helpers
use_iucn <- function() {
  rredlist::rl_use_iucn()
}
