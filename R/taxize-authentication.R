#' @title taxize authentication
#' 
#' @description Help on authentication
#'
#' @name taxize-authentication
#' @aliases authentication
#' @seealso [key_helpers()]
#' 
#' @section What is an API?:
#' An API is an Application Programming Interface. The term "API" can be used
#' for lots of scenarios, but in this case we're talking about web APIs,
#' or APIs (interfaces) to web resources. \pkg{taxize} interacts with
#' remote databases on the web via their APIs. You don't need to worry
#' about the details of how that all works; just know that some of them
#' require authentication and some do not.
#'
#' @section What are API keys?:
#' For those APIs that require authentication, the way that's typically done
#' is through API keys: alphanumeric strings of variable lengths that are
#' supplied with a request to an API.
#'
#' \pkg{taxize} won't get these keys for you; rather, you have to
#' go get a key for each service, but we do provide information on how
#' to get those keys. See [key_helpers()] for help on how to
#' obtain keys for this package.
#'
#' @section Using API keys:
#' You can store API keys as R options in your `.Rprofile` file, or
#' as environment variables in either your `.Renviron` file or
#' `.bash_profile` file, o`.zshrc` file (if you use oh-my-zsh) or
#' similar. See [Startup] for help on R options and environment
#' variables.
#'
#' Save your API keys with the following names:
#' * Tropicos: R option or env var as 'TROPICOS_KEY'
#' * IUCN: R option or env var as 'IUCN_REDLIST_KEY'
#' * ENTREZ: R option or env var as 'ENTREZ_KEY'
#'
#' If you save in .Renviron it looks like: `ENTREZ_KEY=somekey`
#'
#' If you save in a .bash_profile, .zshrc, or similar file it looks like:
#' `export ENTREZ_KEY=somekey`
#'
#' If you save in a .Rprofile it looks like: `options(ENTREZ_KEY = "somekey")`
#'
#' Remember to restart your R session (and to start a new shell window/tab
#' if you're using the shell) to take advantage of the new R options
#' or environment variables.
#'
#' We strongly recommend using environment variables
#' (<https://en.wikipedia.org/wiki/Environment_variable>) over R options
#' because environment variables are widely used across programming
#' languages, operating systems, and computing environments; whereas
#' R options are specific to R.
#'
#' Note that NCBI Entrez doesn't require that you use an API key,
#' but you do get a higher rate limit with a key (more requests per
#' time period), from 3 to 10 requests per second, so do get one.
NULL
