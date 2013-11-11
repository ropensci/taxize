.onAttach <- function(...) {
  packageStartupMessage("\n\nNew to taxize? Tutorial at http://ropensci.org/tutorials/taxize_tutorial.html \nAPI key names have changed. Use tropicosApiKey, eolApiKey, ubioApiKey, and pmApiKey in your .Rprofile file. \nUse suppressPackageStartupMessages() to suppress these startup messages in the future\n")
} 