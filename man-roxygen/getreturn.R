#' @return A vector of taxonomic identifiers as an S3 class.
#' If a taxon is not found an \code{NA} is given. If more than one identifier is found the
#' function asks for user input if \code{ask = TRUE}, otherwise returns \code{NA}.
#' If \code{ask=FALSE} and \code{rows} does not equal \code{NA}, then a data.frame is
#' given back, but not of the uid class, which you can't pass on to other functions
#' as you normally can.
#'
#' Comes with the following attributes:
#' \itemize{
#'  \item \emph{match} (character) - the reason for NA, either 'not found', 'found' or
#'  if \code{ask = FALSE} then 'NA due to ask=FALSE')
#'  \item \emph{multiple_matches} (logical) - Whether multiple matches were returned by
#'  the data source. This can be \code{TRUE}, even if you get 1 name back because we try
#'  to pattern match the name to see if there's any direct matches. So sometimes
#'  this attribute is \code{TRUE}, as well as \code{pattern_match}, which then returns 1
#'  resulting name without user prompt.
#'  \item \emph{pattern_match} (logical) - Whether a pattern match was made. If \code{TRUE}
#'  then \code{multiple_matches} must be \code{TRUE}, and we found a perfect match to your
#'  name, ignoring case. If \code{FALSE}
#' }
