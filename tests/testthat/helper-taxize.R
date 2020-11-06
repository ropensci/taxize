sw <- function(x) suppressWarnings(x)
sm <- function(x) suppressMessages(x)

# set up vcr
library("vcr")
invisible(vcr::vcr_configure(
  dir = "../fixtures",
  filter_sensitive_data = list(
    "<<rredlist_api_token>>" = Sys.getenv("IUCN_REDLIST_KEY"),
    "<<entrez_api_token>>" = Sys.getenv("ENTREZ_KEY"),
    "<<tropicos_api_token>>" = Sys.getenv("TROPICOS_KEY"),
    "<<natureserve_api_token>>" = Sys.getenv("NATURE_SERVE_KEY")
  )
))
vcr::check_cassette_names()

# suppress messages in progressor class in get_* fxns
taxize_options(TRUE, quiet = TRUE)

has_internet <- function() {
  z <- try(suppressWarnings(readLines('https://www.google.com', n = 1)),
    silent = TRUE)
  !inherits(z, "try-error")
}

skip_if_net_down <- function() {
  if (has_internet()) {
    return()
  }
  testthat::skip("no internet")
}
