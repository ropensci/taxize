#' Retrieve gene sequences from NCBI by taxon name and gene names.
#'
#' THIS FUNCTION IS DEFUNCT.
#'
#' @export
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @rdname ncbi_getbyname-defunct
#' @keywords internal
ncbi_getbyname <- function(...) {
  .Defunct("ncbi_byname", "traits", msg = "This function is defunct. See traits::ncbi_byname()")
}

#' Retrieve gene sequences from NCBI by accession number.
#'
#' THIS FUNCTION IS DEFUNCT.
#'
#' @export
#' @keywords internal
#' @rdname get_seqs-defunct
get_seqs <- function(...) {
  .Defunct("ncbi_getbyname", "taxize", "Function name changed. See ncbi_getbyname")
}
