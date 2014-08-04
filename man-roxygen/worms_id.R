#' @param ids (numeric) One or more WoRMS AphidID's for a taxon.
#' @param opts (character) a named list of elements that are passed to the curlPerform function 
#'    which actually invokes the SOAP method. These options control aspects of the HTTP request, 
#'    including debugging information that is displayed on the console, 
#'    e.g. .opts = list(verbose = TRUE)
#' @param iface Interface to WoRMS SOAP API methods. By default we use a previously created object.
#'    If you want to create a new one, use \code{worms_gen_iface}, assign the output to an object, 
#'    then pass it into any \code{worms_*} function. in the \code{iface} parameter.
#' @param ... Further args passed on to \code{SSOAP::.SOAP}.