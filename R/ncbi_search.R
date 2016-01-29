#' Search for gene sequences available for taxa from NCBI.
#'
#' THIS FUNCTION IS DEFUNCT.
#'
#' @rdname ncbi_search-defunct
#' @keywords internal
#' @export
ncbi_search <- function(...) {
  .Defunct("ncbi_searcher", "traits", msg = "This function is defunct. See traits::ncbi_searcher()")
}

#' Retrieve gene sequences from NCBI by accession number.
#'
#' THIS FUNCTION IS DEFUNCT.
#'
#' @export
#' @keywords internal
#' @rdname get_genes_avail-defunct
get_genes_avail <- function(...) {
  .Defunct("ncbi_search", "taxize", "Function name changed. See ncbi_search")
}
