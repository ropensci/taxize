#' Search for gene sequences available for a species from NCBI.
#'
#' @import XML httr stringr data.table
#' @template ncbi
#' @param id (character) Taxonomic id to search for. Not compatible with argument \code{taxa}.
#' @param limit Number of sequences to search for and return. Max of 10,000.
#'    If you search for 6000 records, and only 5000 are found, you will of course
#'    only get 5000 back.
#' @details Removes predicted sequences so you don't have to remove them.
#'     Predicted sequences are those with accession numbers that have "XM_" or
#' 		"XR_" prefixes.
#' @return Data.frame of results.
#' @seealso \code{\link[taxize]{ncbi_getbyid}}, \code{\link[taxize]{ncbi_getbyname}}
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @examples \donttest{
#' # A single species
#' out <- ncbi_search(taxa="Umbra limi", seqrange = "1:2000")
#' # Get the same species information using a taxonomy id 
#' out <- ncbi_search(id = "75935", seqrange = "1:2000")
#' # If the taxon name is unique, using the taxon name and id are equivalent
#' all(ncbi_search(id = "75935") ==  ncbi_search(taxa="Umbra limi"))
#' # If the taxon name is not unique, use taxon id
#' # "266948" is the uid for the butterfly genus, but there is also a genus of orchids with the same name
#' nrow(ncbi_search(id = "266948")) ==  nrow(ncbi_search(taxa="Satyrium"))
#' # get list of genes available, removing non-unique
#' unique(out$gene_desc)
#' # does the string 'RAG1' exist in any of the gene names
#' out[grep("RAG1", out$gene_desc, ignore.case=TRUE),]
#'
#' # A single species without records in NCBI
#' out <- ncbi_search(taxa="Sequoia wellingtonia", seqrange="1:2000", getrelated=TRUE)
#'
#' # Many species, can run in parallel or not using plyr
#' species <- c("Salvelinus alpinus","Ictalurus nebulosus","Carassius auratus")
#' out2 <- ncbi_search(taxa=species, seqrange = "1:2000")
#' lapply(out2, head) # see heads of all
#' library("plyr")
#' out2df <- ldply(out2) # make data.frame of all
#' unique(out2df$gene_desc) # get list of genes available, removing non-unique
#' out2df[grep("60S ribosomal protein", out2df$gene_desc, ignore.case=TRUE),] # search across all
#' }
#' @export
ncbi_search <- function(taxa = NULL, id = NULL, seqrange="1:3000", getrelated=FALSE, limit = 500,
                        verbose=TRUE)
{
  # Argument validation 
  if (sum(c(is.null(taxa), is.null(id))) != 1) {
    stop("Either taxa or id must be specified, but not both")
  }
  if ((!is.null(id)) && getrelated) warning("option 'getrelated' has no effect when used with option 'id'.")
  
  # Function to parse results from http query
  parseres <- function(x){
    outsum <- xpathApply(content(x, as="parsed"), "//eSummaryResult")[[1]]
    names <- sapply(getNodeSet(outsum[[1]], "//Item"), xmlGetAttr, name="Name") # gets names of values in summary
    predicted <- as.character(sapply(getNodeSet(outsum, "//Item"), xmlValue)[str_detect(names, "Caption")]) #  get access numbers
    predicted <- sapply(predicted, function(x) strsplit(x, "_")[[1]][[1]], USE.NAMES=FALSE)
    length_ <- as.numeric(sapply(getNodeSet(outsum, "//Item"), xmlValue)[str_detect(names, "Length")]) # gets seq lengths
    gis <- as.numeric(sapply(getNodeSet(outsum, "//Item"), xmlValue)[str_detect(names, "Gi")]) # gets GI numbers
    spnames <- sapply(getNodeSet(outsum, "//Item"), xmlValue)[str_detect(names, "Title")] # gets seq lengths # get spp names
    spused <- sapply(spnames, function(x) paste(str_split(x, " ")[[1]][1:2], sep="", collapse=" "), USE.NAMES=FALSE)
    genesavail <- sapply(spnames, function(x) paste(str_split(x, " ")[[1]][-c(1:2)], sep="", collapse=" "), USE.NAMES=FALSE)
    df <- data.frame(spused=spused, length=length_, genesavail=genesavail, access_num=predicted, ids=gis, stringsAsFactors=FALSE)
    return( df[!df$access_num %in% c("XM","XR"),] )
  }
  
  foo <- function(xx){
    url_esearch <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
    url_esummary <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi"
    mssg(verbose, paste("Working on ", xx, "...", sep=""))
    mssg(verbose, "...retrieving sequence IDs...")
    
    if (is.null(id)) {
      query_term <- paste(xx, "[Organism] AND", seqrange, "[SLEN]", collapse=" ")    
    } else {
      query_term <- paste0("xXarbitraryXx[porgn:__txid", xx, "] AND ", seqrange, " [SLEN]")
    }
    query <- list(db = "nuccore", retmax = limit, term = query_term)      
    query_init <- GET(url_esearch, query=query)
    stop_for_status(query_init)
    out <- xpathApply(content(query_init, as="parsed"), "//eSearchResult")[[1]]
    if( as.numeric(xmlValue(xpathApply(out, "//Count")[[1]]))==0 ){
      if (!is.null(id)) {
        mssg(verbose, paste("no sequences for ", xx, sep=""))
        return(NULL)
      }
      mssg(verbose, paste("no sequences for ", xx, " - getting other sp.", sep=""))
      if(getrelated == FALSE){
        mssg(verbose, paste("no sequences for ", xx, sep=""))
        df <- data.frame(xx, NA, NA, NA, NA)
      } else
      {
        mssg(verbose, "...retrieving sequence IDs for related species...")
        newname <- strsplit(xx, " ")[[1]][[1]]
        query <- list(db = "nuccore", retmax = limit,
                      term = paste(newname, "[Organism] AND", seqrange, "[SLEN]", collapse=" "))
        query_init2 <- GET(url_esearch, query=query)
        stop_for_status(query_init2)
        out <- xpathApply(content(query_init2, "parsed"), "//eSearchResult")[[1]]
        if( as.numeric(xmlValue(xpathApply(out, "//Count")[[1]]))==0 ){
          mssg(verbose, paste("no sequences for ", xx, " or ", newname, sep=""))
          df <- data.frame(xx, NA, NA, NA, NA)
        }
{
  ids <- xpathApply(out, "//IdList//Id") # Get sequence IDs in list
  ids_ <- as.numeric(sapply(ids, xmlValue))  # Get sequence ID values
  mssg(verbose, "...retrieving available genes and their lengths...")
  
  actualnum <- length(ids_)
  if(actualnum > 10000){
    q <- list(db = "nucleotide")
    getstart <- seq(from=1, to=actualnum, by=10000)
    getnum <- c(rep(10000, length(getstart)-1), actualnum-sum(rep(10000, length(getstart)-1)))
    iterlist = list()
    for(i in seq_along(getstart)){
      q$id = paste(ids_[getstart[i]:(getstart[i]+(getnum[i]-1))], collapse=" ")
      q$retstart <- getstart[i]
      q$retmax <- getnum[i]
      query_res <- POST(url_esummary, body=q)
      stop_for_status(query_res)
      iterlist[[i]] <- parseres(query_res)
    }
    df <- data.frame(rbindlist(iterlist))
  } else
  {
    q <- list(db = "nucleotide", id = paste(ids_, collapse=" "))
    query_res <- POST(url_esummary, body=q)
    stop_for_status(query_res)
    df <- parseres(query_res)
  }
}
      }
    } else
    {
      ids <- xpathApply(out, "//IdList//Id") # Get sequence IDs in list
      ids_ <- as.numeric(sapply(ids, xmlValue))  # Get sequence ID values
      mssg(verbose, "...retrieving available genes and their lengths...")
      
      actualnum <- length(ids_)
      if(actualnum > 10000){
        q <- list(db = "nucleotide")
        getstart <- seq(from=1, to=actualnum, by=10000)
        getnum <- c(rep(10000, length(getstart)-1), actualnum-sum(rep(10000, length(getstart)-1)))
        iterlist = list()
        for(i in seq_along(getstart)){
          q$id = paste(ids_[getstart[i]:(getstart[i]+(getnum[i]-1))], collapse=" ")
          q$retstart <- getstart[i]
          q$retmax <- getnum[i]
          query_res <- POST(url_esummary, body=q)
          stop_for_status(query_res)
          iterlist[[i]] <- parseres(query_res)
        }
        df <- data.frame(rbindlist(iterlist))
      } else
      {
        q <- list(db = "nucleotide", id = paste(ids_, collapse=" "))
        query_res <- POST(url_esummary, body=q)
        stop_for_status(query_res)
        df <- parseres(query_res)
      }
    }
mssg(verbose, "...done.")
if(nrow(df) < 1){
  df <- data.frame(taxon=NA,length=NA,gene_desc=NA,acc_no=NA,gi_no=NA)
} else {
  names(df) <- c("taxon","length","gene_desc","acc_no","gi_no")
}
return( df )
  }

foo_safe <- plyr::failwith(NULL, foo)
if (length(c(taxa, id)) == 1) {
  foo_safe(c(taxa, id))
} else {
  lapply(c(taxa, id), foo_safe)
}
}

#' Retrieve gene sequences from NCBI by accession number.
#'
#' Function name changed to ncbi_search.
#'
#' @param taxa Scientific name to search for (character).
#' @param seqrange Sequence range, as e.g., "1:1000" (character).
#' @param getrelated Logical, if TRUE, gets the longest sequences of a species
#'   	in the same genus as the one searched for. If FALSE, get's nothing.
#' @param verbose logical; If TRUE (default), informative messages printed.
#' @export
#' @keywords internal
#' @rdname get_genes_avail-deprecated

get_genes_avail <- function(taxa, seqrange="1:3000", getrelated=FALSE, verbose=TRUE)
{
  .Deprecated("ncbi_search", "taxize", "Function name changed. See ncbi_search", "get_genes_avail")
}
