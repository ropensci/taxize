#' Retrieve gene sequences from NCBI by accession number.
#'
#' @import XML httr stringr data.table
#' @export
#' @param ids (character) GenBank ids to search for.
#' @param format (character) Return type, e.g., \code{"fasta"}
#' @param verbose (logical) If \code{TRUE} (default), informative messages printed.
#' @details Removes predicted sequences so you don't have to remove them.
#'   	Predicted sequences are those with accession numbers that have "XM_" or
#' 		"XR_" prefixes. This function retrieves one sequences for each species,
#'   	picking the longest available for the given gene.
#' @return Data.frame of results.
#' @seealso \code{\link[taxize]{ncbi_search}}, \code{\link[taxize]{ncbi_getbyname}}
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @examples \dontrun{
#' # A single gene
#' ncbi_getbyid(ids="360040093", format="fasta")
#'
#' # Many genes (with different accession numbers)
#' ncbi_getbyid(ids=c("360040093","347448433"), format="fasta")
#' }
ncbi_getbyid <- function(ids, format="fasta", verbose=TRUE)
{
	mssg(verbose, "Retrieving sequence IDs...")

	ids <- paste(ids, collapse=",")
	queryseq <- list(db = "sequences", id = ids, rettype = format, retmode = "text")
	tt <- GET("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi", query = queryseq)
  stop_for_status(tt)
	outseq <- content(tt, as="text")

	outseq2 <- str_split(outseq, '>')[[1]][-1]

	foo <- function(x){
		temp <- paste(">", x, sep="")
		seq <- str_replace_all(str_split(str_replace(temp[[1]], "\n", "<<<"), "<<<")[[1]][[2]], "\n", "")
		idaccess <- str_split(x, "\\|")[[1]][c(2,4)]
		desc <- str_split(str_split(x, "\\|")[[1]][[5]], "\n")[[1]][[1]]
		outt <- list(desc, as.character(idaccess[1]), idaccess[2], nchar(seq), seq)
		spused <- paste(str_split(str_trim(str_split(temp, "\\|")[[1]][[5]], "both"), " ")[[1]][1:2], sep="", collapse=" ")
		outoutout <- data.frame(spused=spused, outt)
		names(outoutout) <- c("taxon","gene_desc","gi_no","acc_no","length","sequence")
		outoutout
	}
	df <- data.frame(rbindlist(lapply(outseq2, foo)))
	mssg(verbose, "...done")
	return(df)
}

#' Retrieve gene sequences from NCBI by accession number.
#'
#' Function name changed to ncbi_getbyid.
#'
#' @param ids GenBank ids to search for (character).
#' @param format Return type, e.g., "fasta"
#' @param verbose logical; If TRUE (default), informative messages printed.
#' @export
#' @keywords internal
#' @rdname get_genes-deprecated

get_genes <- function(ids, format="fasta", verbose=TRUE)
{
  .Deprecated("ncbi_getbyid", "taxize", "Function name changed. See ncbi_getbyid", "get_genes")
}
