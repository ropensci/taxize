taxize_env <- new.env()
taxize_env$options <- list()

#' taxize options
#'
#' @export
#' @param taxon_state_messages (logical) suppress messages? default: `NULL`
#' (same as setting `FALSE`). Set to `TRUE` to suppress messages, and `FALSE`
#' to not suppress messages
#' @param ncbi_sleep (numeric/integer) number of seconds to sleep between
#' NCBI ENTREZ http requests. applies to the functions: [classification()],
#' [comm2sci()], [genbank2uid()], [get_uid()] and [ncbi_children()].
#' defaults: 0.334 (without API key) or 0.101 (with API key). minimum value
#' can not be less than 0.101
#' @param quiet (logical) quiet informational output from this function.
#' default: `TRUE`
#' @examples \dontrun{
#' taxize_options()
#' taxize_options(FALSE)
#' taxize_options(TRUE)
#' taxize_options(ncbi_sleep = 0.4)
#' taxize_options(taxon_state_messages = TRUE, ncbi_sleep = 0.4)
#' }
taxize_options <- function(taxon_state_messages = NULL, ncbi_sleep = NULL,
  quiet = FALSE) {

  assert(taxon_state_messages, 'logical')
  taxize_env$options$taxon_state_messages <- taxon_state_messages
  if (!is.null(ncbi_sleep)) {
    assert(ncbi_sleep, c('numeric', 'integer'))
    if (!ncbi_sleep > 0.101) stop("'ncbi_sleep' must be > 0.101")
  }
  taxize_env$options$ncbi_sleep_sec <- ncbi_sleep
  tseo <- taxize_env$options
  if (!quiet) cat("taxize options", sep = "\n")
  if (length(tseo) == 0) return(cat(""))
  for (i in seq_along(tseo)) {
    if (!quiet) cat(sprintf("  %s: %s", names(tseo)[i], tseo[[i]]),
      sep = "\n")
  }
}
