#' Retrieve gene sequences from NCBI by accession number.
#' 
#' @import XML httr stringr
#' @param taxon_name Scientific name to search for (character).
#' @param seqrange Sequence range, as e.g., "1:1000" (character).
#' @param getrelated Logical, if TRUE, gets the longest sequences of a species 
#' 		in the same genus as the one searched for. If FALSE, get's nothing.
#' @details Removes predicted sequences so you don't have to remove them. 
#' 		Predicted sequences are those with accession numbers that have "XM_" or 
#' 		"XR_" prefixes. 
#' @return Data.frame of results. 
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @examples \dontrun{
#' # A single gene
#' get_genes(ids="360040093", format="fasta")
#' 
#' # Many genes (with different accession numbers) for the same species
#' get_genes(ids=c("360040093","347448433"), format="fasta")
#' }
#' @export
get_genes <- function(ids, format="fasta")
{
	message("Retrieving sequence IDs...")
	
	ids <- paste(ids, collapse=",")
	queryseq <- list(db = "sequences", id = ids, rettype = format, retmode = "text")
	outseq <- content(
		GET("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi", query = queryseq))  
	
	outseq2 <- str_split(outseq, '>')[[1]][-1]
	
	foo <- function(x){
		temp <- paste(">", x, sep="")
		seq <- str_replace_all(str_split(str_replace(temp[[1]], "\n", "<<<"), "<<<")[[1]][[2]], "\n", "")
		idaccess <- str_split(x, "\\|")[[1]][c(2,4)]
		outt <- list(as.character(idaccess[1]), idaccess[2], nchar(seq), seq)
		spused <- paste(str_split(str_trim(str_split(temp, "\\|")[[1]][[5]], "both"), " ")[[1]][1:2], sep="", collapse=" ")
		outoutout <- data.frame(spused=spused, outt)
		names(outoutout) <- c("sp","ids","accnum","length","seq")
		outoutout
	}
	df <- ldply(outseq2, foo)
	message("...done")
	return(df)
}