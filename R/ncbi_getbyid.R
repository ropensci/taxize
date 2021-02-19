#' Retrieve gene sequences from NCBI by accession number.
#'
#' THIS FUNCTION IS DEFUNCT.
#'
#' @export
#' @author Scott Chamberlain
#' @rdname ncbi_getbyid-defunct
#' @keywords internal
ncbi_getbyid <- function(...) {
  .Defunct("ncbi_byid", "traits",
    msg = "This function is defunct. See traits::ncbi_byid()")
}

#' Retrieve gene sequences from NCBI by accession number.
#'
#' THIS FUNCTION IS DEFUNCT.
#'
#' @export
#' @keywords internal
#' @rdname get_genes-defunct
get_genes <- function(...) {
  .Defunct("ncbi_getbyid", "taxize", "Function name changed. See ncbi_getbyid")
}
