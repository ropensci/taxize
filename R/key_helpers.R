
#' @importFrom utils browseURL
#' @title Helpers to set up authentication for the different providers.
#'
#' @description Sets up authentication to diverse providers by providing the user a detailed prompt.
#'
#' @name key_helpers
#' @aliases NULL
NULL

#' @section `use_tropicos()`:
#' Browse Tropicos API key request URL and
#'  provides instruction on how to store the key.
#' @export
#' @rdname key_helpers
use_tropicos <- function(){
  if(interactive()){
    utils::browseURL("http://services.tropicos.org/help?requestkey")
  }
  
  message("After getting your key set it as TROPICOS_KEY in .Renviron.\n TROPICOS_KEY='youractualkeynotthisstring'\n For that, use usethis::edit_r_environ().")
  
  invisible("http://services.tropicos.org/help?requestkey")
}


#' @section `use_eol()`:
#' Browse EOL to help make an API key request and
#'  provides instruction on how to store the key.
#' There's no direct URL to request a key, one first needs 
#' to log in or register and then to generate a key from one's Preferences
#' page.
#'  
#' @export
#' @rdname key_helpers
use_eol <- function(){
  if(interactive()){
    utils::browseURL("http://eol.org/info/api_overview")
  }
  
  message("Generate your key in your (brand-new) account's Preferences page. \nAfter generating your key set it as EOL_KEY in .Renviron.\n EOL_KEY='youractualkeynotthisstring'\n For that, use usethis::edit_r_environ().")
  
  invisible("http://eol.org/info/api_overview")
}



#' @section `use_entrez()`:
#' Browse NCBI Entrez to help make an API key request and
#'  provides instruction on how to store the key.
#' There's no direct URL to request a key, one first needs 
#' to log in or register and then to generate a key from one's account.
#' 
#' Note that NCBI Entrez doesn't require that you use an API key, 
#' but you should get higher rate limit with a key, so do get one.
#'  
#' @export
#' @rdname key_helpers
use_entrez <- function(){
  if(interactive()){
    utils::browseURL("https://www.ncbi.nlm.nih.gov/account/")
  }
  
  message("Create your key from your (brand-new) account's. \nAfter generating your key set it as ENTREZ_KEY in .Renviron.\n ENTREZ_KEY='youractualkeynotthisstring'\n For that, use usethis::edit_r_environ().")
  
  invisible("https://www.ncbi.nlm.nih.gov/account/")
}

#' @section `use_iucn()`:
#' Browse IUCN Red List API key request URL and
#'  provides instruction on how to store the key.
#' @export
#' @rdname key_helpers
use_iucn <- function(){
  if(interactive()){
    utils::browseURL("http://apiv3.iucnredlist.org/api/v3/token")
  }
  
  message("After getting your key set it as IUCN_REDLIST_KEY in .Renviron.\n IUCN_REDLIST_KEY='youractualkeynotthisstring'\n For that, use usethis::edit_r_environ().")
  
  invisible("http://apiv3.iucnredlist.org/api/v3/token")
}