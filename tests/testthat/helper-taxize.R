sw <- function(x) suppressWarnings(x)
sm <- function(x) suppressMessages(x)

# set up vcr
library("vcr")
invisible(vcr::vcr_configure(
  dir = "../fixtures",
  filter_sensitive_data = list(
    "<<rredlist_api_token>>" = Sys.getenv('IUCN_REDLIST_KEY'),
    "<<entrez_api_token>>" = Sys.getenv('ENTREZ_KEY'),
    "<<eol_api_token>>" = Sys.getenv('EOL_KEY'),
    "<<tropicos_api_token>>" = Sys.getenv('TROPICOS_KEY')
  )
))
