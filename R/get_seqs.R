#' Retrieve nucleotide sequences from NCBI.
#' 
#' This function retrieves one sequences for each species, picking the longest
#' 		available for the given gene. 
#' 
#' @import XML httr stringr
#' @param taxon_name Scientific name to search for (character).
#' @param gene Gene (character) or genes (character vector) to search for.
#' @param seqrange Sequence range, as e.g., "1:1000" (character).
#' @param getrelated Logical, if TRUE, gets the longest sequences of a species 
#' 		in the same genus as the one searched for. If FALSE, get's nothing.
#' @param writetodf Write resulting data.frame of results to a file on your
#' 		machine (logical).
#' @param filetowriteto If writetodf=TRUE, then specify the file name. Default=T.
#' @details Removes predicted sequences so you don't have to remove them. 
#' 		Predicted sequences are those with accession numbers that have "XM_" or 
#' 		"XR_" prefixes. 
#' @return Data.frame of results. 
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @examples \dontrun{
#' # A single species
#' get_seqs(taxon_name="Acipenser brevirostrum", gene = c("coi", "co1"), 
#'		seqrange = "1:3000", getrelated=T, writetodf=F)
#' 
#' # Many species, can run in parallel or not using plyr
#' species <- c("Colletes similis","Halictus ligatus","Perdita trisignata")
#' llply(species, get_seqs, gene = c("coi", "co1"), seqrange = "1:2000", 
#'    getrelated=T, writetodf=F)
#' }
#' @export
get_seqs <- function(taxon_name, gene, seqrange, getrelated, writetodf=TRUE, 
                     filetowriteto)
{
	message(paste("Working on ", taxon_name, "...", sep=""))
	message("...retrieving sequence IDs...")
	
	if(length(gene) > 1){ genes_ <- paste(gene, sep="", collapse=" OR ") } else
		{ genes_ <- paste(gene, sep="", collapse=" ") }
	genes_ <- paste("(", genes_, ")")
	
	query <- list(db = "nuccore", term = paste(taxon_name, "[Organism] AND", genes_, "AND", seqrange, "[SLEN]", collapse=" "), RetMax=500)
	
	out <- 
		xpathApply(content(GET("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi", query=query), "parsed"), "//eSearchResult")[[1]]
	if( as.numeric(xmlValue(xpathApply(out, "//Count")[[1]]))==0 ){
		message(paste("no sequences of ", gene, " for ", taxon_name, " - getting other sp.", sep=""))
		if(getrelated == FALSE){
			message(paste("no sequences of ", gene, " for ", taxon_name, sep=""))
			outoutout <- data.frame(list(taxon_name, "NA", "NA", "NA", "NA", "NA", "NA"))
			names(outoutout) <- NULL
		} else
		{
			message("...retrieving sequence IDs for related species...")
			newname <- strsplit(taxon_name, " ")[[1]][[1]]
			query <- list(db = "nuccore", term = paste(newname, "[Organism] AND", genes_, "AND", seqrange, "[SLEN]", collapse=" "), RetMax=500)
			out <- 
				xpathApply(content(GET("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi", query=query), "parsed"), "//eSearchResult")[[1]]
			if( as.numeric(xmlValue(xpathApply(out, "//Count")[[1]]))==0 ){
				message(paste("no sequences of ", gene, " for ", taxon_name, " or ", newname, sep=""))
				outoutout <- data.frame(list(taxon_name, "NA", "NA", "NA", "NA", "NA", "NA"))
				names(outoutout) <- NULL
			} else
			{
				ids <- xpathApply(out, "//IdList//Id")
				ids_ <- as.numeric(sapply(ids, xmlValue))
				
				## For each species = get GI number with longest sequence
				message("...retrieving sequence ID with longest sequence length...")
				querysum <- list(db = "nucleotide", id = paste(ids_, collapse=" ")) # construct query for species
				outsum <- 
						xpathApply(content(GET("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi", query=querysum), "parsed"), "//eSummaryResult")[[1]]
				names <- sapply(getNodeSet(outsum[[1]], "//Item"), xmlGetAttr, name="Name") # gets names of values in summary
				predicted <- as.character(sapply(getNodeSet(outsum, "//Item"), xmlValue)[str_detect(names, "Caption")]) #  get access numbers
				predicted <- sapply(predicted, function(x) strsplit(x, "_")[[1]][[1]], USE.NAMES=F)
				length_ <- as.numeric(sapply(getNodeSet(outsum, "//Item"), xmlValue)[str_detect(names, "Length")]) # gets seq lengths
				gis <- as.numeric(sapply(getNodeSet(outsum, "//Item"), xmlValue)[str_detect(names, "Gi")]) # gets GI numbers
				spnames <- sapply(getNodeSet(outsum, "//Item"), xmlValue)[str_detect(names, "Title")] # gets seq lengths # get spp names
				df <- data.frame(gis=gis, length=length_, spnames=laply(spnames, c), predicted=predicted) # makes data frame
				df <- df[!df$predicted %in% c("XM","XR"),] # remove predicted sequences
				gisuse <- df[which.max(x=df$length),] # picks longest sequnence length
				if(nrow(gisuse)>1){gisuse <- gisuse[sample(nrow(gisuse), 1), ]} else 
					{gisuse <- gisuse}
				
				## Get sequence from previous
				message("...retrieving sequence...")
				queryseq <- list(db = "sequences", id = gisuse[,1], rettype = "fasta", retmode = "text")
				outseq <- content(GET("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi", query = queryseq), as="text")
				seq <- str_replace_all(str_split(str_replace(outseq, "\n", "<<<"), "<<<")[[1]][[2]], "\n", "")
				accessnum <- str_split(outseq, "\\|")[[1]][4]
				outt <- list(taxon_name, as.character(gisuse[,3]), gisuse[,1], accessnum, gisuse[,2], seq)
				
				spused <- paste(str_split(outt[[2]], " ")[[1]][1:2], sep="", collapse=" ")
				outoutout <- data.frame(outt, spused=spused)
				names(outoutout) <- NULL
			}
		}
	} else
	{
		ids <- xpathApply(out, "//IdList//Id") # Get sequence IDs in list
		ids_ <- as.numeric(sapply(ids, xmlValue))  # Get sequence ID values
		
		## For each species = get GI number with longest sequence
		message("...retrieving sequence ID with longest sequence length...")
		querysum <- list(db = "nucleotide", id = paste(ids_, collapse=" ")) # construct query for species
		outsum <- xpathApply(content( # API call
			GET("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi", 
					query=querysum), "parsed"), "//eSummaryResult")[[1]]
		names <- sapply(getNodeSet(outsum[[1]], "//Item"), xmlGetAttr, name="Name") # gets names of values in summary
		predicted <- as.character(sapply(getNodeSet(outsum, "//Item"), xmlValue)[str_detect(names, "Caption")]) #  get access numbers
		predicted <- sapply(predicted, function(x) strsplit(x, "_")[[1]][[1]], USE.NAMES=F)
		length_ <- as.numeric(sapply(getNodeSet(outsum, "//Item"), xmlValue)[str_detect(names, "Length")]) # gets seq lengths
		gis <- as.numeric(sapply(getNodeSet(outsum, "//Item"), xmlValue)[str_detect(names, "Gi")]) # gets GI numbers
		spnames <- sapply(getNodeSet(outsum, "//Item"), xmlValue)[str_detect(names, "Title")] # gets seq lengths # get spp names
		df <- data.frame(gis=gis, length=length_, spnames=laply(spnames, c), predicted=predicted) # makes data frame
		df <- df[!df$predicted %in% c("XM","XR"),] # remove predicted sequences
		gisuse <- df[which.max(x=df$length),] # picks longest sequnence length
		if(nrow(gisuse)>1){gisuse <- gisuse[sample(nrow(gisuse), 1), ]} else 
			{gisuse <- gisuse}
		
		## Get sequence from previous
		message("...retrieving sequence...")
		queryseq <- list(db = "sequences", id = gisuse[,1], rettype = "fasta", retmode = "text")
		outseq <- content(GET("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi", query = queryseq), as="text")
		seq <- str_replace_all(str_split(str_replace(outseq, "\n", "<<<"), "<<<")[[1]][[2]], "\n", "")
		accessnum <- str_split(outseq, "\\|")[[1]][4]
		outt <- list(taxon_name, as.character(gisuse[,3]), gisuse[,1], accessnum, gisuse[,2], seq)
		
		spused <- paste(str_split(outt[[2]], " ")[[1]][1:2], sep="", collapse=" ")
		outoutout <- data.frame(outt, spused=spused)
		names(outoutout) <- NULL
	}
	
	message("...done.")
	if(writetodf){
		write.table(outoutout, file = filetowriteto, append=T, row.names=F)
	} else
	{
		names(outoutout) <- 
			c("taxon", "gene_desc", "gi_no", "acc_no", "length", "sequence", "spused")
		outoutout
	}
}