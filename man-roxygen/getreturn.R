#' @return A vector of taxonomic identifiers as an S3 class.
#' If a taxon is not found an \code{NA} is given. If more than one identifier
#' is found the function asks for user input if \code{ask = TRUE}, otherwise
#' returns \code{NA}. If \code{ask=FALSE} and \code{rows} does not equal
#' \code{NA}, then a data.frame is given back, but not of the uid class, which
#' you can't pass on to other functions as you normally can.
#'
#' See \code{\link{get_id_details}} for further details including
#' attributes and exceptions
