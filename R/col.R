col_defunct_mssg = "COL API is no longer reliable; all COL fxns removed"

#' COL defunct functions
#' 
#' COL introduced rate limiting recently in 2019 - which has made the API
#' essentially unusable - COL+ is coming soon and we'll incorporate it here
#' when it's stable
#'
#' @name col-defunct
#' @export
#' @keywords internal 
#' @param ... ignored
as.colid <- function(...) .Defunct(msg = col_defunct_mssg)

#' @export
#' @rdname col-defunct
col_children <- function(...) .Defunct(msg = col_defunct_mssg)

#' @export
#' @rdname col-defunct
col_classification <- function(...) .Defunct(msg = col_defunct_mssg)

#' @export
#' @rdname col-defunct
col_downstream <- function(...) .Defunct(msg = col_defunct_mssg)

#' @export
#' @rdname col-defunct
col_search <- function(...) .Defunct(msg = col_defunct_mssg)

#' @export
#' @rdname col-defunct
get_colid <- function(...) .Defunct(msg = "See ?get_col")

#' @export
#' @rdname col-defunct
get_colid_ <- function(...) .Defunct(msg = "See ?get_col_")
