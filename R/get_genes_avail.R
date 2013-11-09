#' Retrieve gene sequences available for a species from NCBI.
#' 
#' This function retrieves one sequences for each species, picking the longest
#' 		available for the given gene. 
#' 
#' @import XML httr stringr
#' @param taxon_name Scientific name to search for (character).
#' @param seqrange Sequence range, as e.g., "1:1000" (character).
#' @param getrelated Logical, if TRUE, gets the longest sequences of a species 
#' 		in the same genus as the one searched for. If FALSE, get's nothing.
#' @param verbose logical; If TRUE (default), informative messages printed.
#' @details Removes predicted sequences so you don't have to remove them. 
#' 		Predicted sequences are those with accession numbers that have "XM_" or 
#' 		"XR_" prefixes. 
#' @return Data.frame of results. 
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @examples \dontrun{
#' # A single species
#' out <- get_genes_avail(taxon_name="Umbra limi", seqrange = "1:2000", 
#'    getrelated=F)
#' # get list of genes available, removing non-unique
#' unique(out$genesavail)
#' # does the string 'RAG1' exist in any of the gene names
#' out[grep("RAG1", out$genesavail, ignore.case=T),] 
#' 
#' # A single species without records in NCBI
#' out <- get_genes_avail(taxon_name="Sequoia wellingtonia", seqrange="1:2000", 
#'    getrelated=T)
#' 
#' # Many species, can run in parallel or not using plyr
#' species <- c("Salvelinus alpinus","Ictalurus nebulosus","Carassius auratus")
#' out2 <- llply(species, get_genes_avail, seqrange = "1:2000", getrelated=F)
#' lapply(out2, head) # see heads of all
#' out2df <- ldply(out2) # make data.frame of all
#' unique(out2df$genesavail) # get list of genes available, removing non-unique
#' out2df[grep("RAG1", out2df$genesavail, ignore.case=T),] # search across all
#' }
#' @export
get_genes_avail <- function(taxon_name, seqrange, getrelated=FALSE, verbose=TRUE)
{
	mssg(verbose, paste("Working on ", taxon_name, "...", sep=""))
	mssg(verbose, "...retrieving sequence IDs...")
	
	query <- list(db = "nuccore", term = paste(taxon_name, "[Organism] AND", seqrange, "[SLEN]", collapse=" "), RetMax=500)
	
	out <- 
		xpathApply(content(GET("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi", query=query), "parsed"), "//eSearchResult")[[1]]
	if( as.numeric(xmlValue(xpathApply(out, "//Count")[[1]]))==0 ){
		mssg(verbose, paste("no sequences for ", taxon_name, " - getting other sp.", sep=""))
		if(getrelated == FALSE){
			mssg(verbose, paste("no sequences for ", taxon_name, sep=""))
			outt <- list(taxon_name, NA, NA, NA, NA, NA)
		} else
		{
			mssg(verbose, "...retrieving sequence IDs for related species...")
			newname <- strsplit(taxon_name, " ")[[1]][[1]]
			query <- list(db = "nuccore", term = paste(newname, "[Organism] AND", seqrange, "[SLEN]", collapse=" "), RetMax=500)
			out <- 
				xpathApply(content(GET("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi", query=query), "parsed"), "//eSearchResult")[[1]]
			if( as.numeric(xmlValue(xpathApply(out, "//Count")[[1]]))==0 ){
				mssg(verbose, paste("no sequences for ", taxon_name, " or ", newname, sep=""))
				outt <- list(taxon_name, NA, NA, NA, NA, NA)
			} else
			{
				ids <- xpathApply(out, "//IdList//Id") # Get sequence IDs in list
				ids_ <- as.numeric(sapply(ids, xmlValue))  # Get sequence ID values
				
				mssg(verbose, "...retrieving available genes and their lengths...")
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
				spused <- sapply(spnames, function(x) paste(str_split(x, " ")[[1]][1:2], sep="", collapse=" "), USE.NAMES=F)
				genesavail <- sapply(spnames, function(x) paste(str_split(x, " ")[[1]][-c(1:2)], sep="", collapse=" "), USE.NAMES=F)
				df <- data.frame(spused=spused, length=length_, genesavail=genesavail, access_num=predicted, ids=gis) # makes data frame
				df <- df[!df$access_num %in% c("XM","XR"),] # remove predicted sequences		
			}
		}
	} else
	{
		ids <- xpathApply(out, "//IdList//Id") # Get sequence IDs in list
		ids_ <- as.numeric(sapply(ids, xmlValue))  # Get sequence ID values
		
		mssg(verbose, "...retrieving available genes and their lengths...")
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
		spused <- sapply(spnames, function(x) paste(str_split(x, " ")[[1]][1:2], sep="", collapse=" "), USE.NAMES=F)
		genesavail <- sapply(spnames, function(x) paste(str_split(x, " ")[[1]][-c(1:2)], sep="", collapse=" "), USE.NAMES=F)
		df <- data.frame(spused=spused, length=length_, genesavail=genesavail, access_num=predicted, ids=gis) # makes data frame
		df <- df[!df$access_num %in% c("XM","XR"),] # remove predicted sequences
	}
	mssg(verbose, "...done.")
	return(df)
}