#' Function to allow user to set timeout on a function call.
#'
#' This is separate from the curl timeout.ms parameter
#'
#' @importFrom R.utils evalWithTimeout
#' @param test_fn The function call to test
#' @param tlimit = 120 A timeout in seconds
#' @param defaultvalue Default value
#' @return If in a loop, passes on to next thing, or if not, then gives back message
#' @examples \dontrun{
#' library("taxize")
#' try_with_timeout(eol_search('Salix'))
#' try_with_timeout(eol_search('Salix'), tlimit=10)
#' }

try_with_timeout <- function(test_fn, tlimit = 30, defaultvalue = "TimedOut") {
  results <- tryCatch(expr = evalWithTimeout(test_fn, timeout = tlimit),
                      TimeoutException = function(ex) defaultvalue)
  if(is(results, "TimedOut")){
    return( defaultvalue )
  } else { results }
}
