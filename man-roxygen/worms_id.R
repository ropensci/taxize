#' @param id (numeric) One or more worms AphidID for a taxon.
#' @param opts (character) a named list of elements that are passed to the curlPerform function 
#'    which actually invokes the SOAP method. These options control aspects of the HTTP request, 
#'    including debugging information that is displayed on the console, 
#'    e.g. .opts = list(verbose = TRUE)
#' @param ... Further args passed on to \code{SSOAP::.SOAP}.