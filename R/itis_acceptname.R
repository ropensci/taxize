#' Retrieve accepted TSN and name
#'
#' @export
#' @param searchtsn One or more TSN for a taxon (numeric/integer)
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#'
#' @return data.frame with with row number equal to input vector length, and
#' with three columns:
#' \itemize{
#'  \item submittedtsn (numeric) - The submitted TSN
#'  \item acceptedname (character) - The accepted name - if the submitted TSN is the
#'  accepted TSN, then this is \code{NA_character_} because ITIS does not return a
#'  name along with the TSN if it's an accepted name. We could make an extra HTTP
#'  request to ITIS, but that means additional time.
#'  \item acceptedtsn (numeric) - The accepted TSN
#'  \item author (character) - taxonomic authority
#' }
#'
#' @examples \dontrun{
#' # TSN accepted - good name
#' itis_acceptname(searchtsn = 208527)
#'
#' # TSN not accepted - input TSN is old
#' itis_acceptname(searchtsn = 504239)
#'
#' # many accepted names
#' ids <- c(18161, 18162, 18163, 18164, 18165, 18166, 46173, 46174,
#' 46178, 46181, 46186, 46193, 46196, 46197, 46200, 46201, 46204,
#' 46207, 46867, 46868)
#' itis_acceptname(searchtsn = ids)
#'
#' # many unaccepted names
#' ids <- c(39087, 46208, 46973, 46976, 46978, 46980, 47295, 47445,
#' 47448, 47512, 47515, 47527, 47546, 47622, 47783, 47786, 47787,
#' 47788, 47835, 47839)
#' itis_acceptname(searchtsn = ids)
#'
#' # many: mix of accepted and unaccepted names
#' ids <- c(18161, 18162, 47527, 47546, 47622, 46200)
#' itis_acceptname(searchtsn = ids)
#' }
itis_acceptname <- function(searchtsn, ...) {
  (dddd <- data.table::setDF(data.table::rbindlist(lapply(searchtsn, function(z) {
    tmp <- ritis::accepted_names(z)
    if (NROW(tmp) == 0) {
      data.frame(submittedtsn = z, acceptedname = NA, acceptedtsn = z,
                 author = NA, stringsAsFactors = FALSE)
    } else {
      data.frame(
        submittedtsn = z,
        stats::setNames(tmp, tolower(names(tmp))),
        stringsAsFactors = FALSE
      )
    }
  }), fill = TRUE, use.names = TRUE)))
}
