#' Search for gene sequences available for taxa from NCBI.
#'
#' @import XML httr stringr data.table
#' @template ncbi
#' @param id (\code{character}) Taxonomic id to search for. Not compatible with argument \code{taxa}.
#' @param limit (\code{numeric}) Number of sequences to search for and return. Max of 10,000.
#'    If you search for 6000 records, and only 5000 are found, you will of course
#'    only get 5000 back.
#' @param entrez_query (\code{character}; length 1) An Entrez-format query to filter results with.
#'   This is useful to search for sequences with specific characteristics. The format is the same
#'   as the one used to seach genbank.
#'   (\url{http://www.ncbi.nlm.nih.gov/books/NBK3837/#EntrezHelp.Entrez_Searching_Options})
#' @param hypothetical (\code{logical}; length 1) If \code{FALSE}, an attempt will be made to not
#'   return hypothetical or predicted sequences judging from accession number prefixs (XM and XR).
#'   This can result in less than the \code{limit} being returned even if there are more sequences
#'   available, since this filtering is done after searching NCBI.
#' @return \code{data.frame} of results if a single input is given. A list of \code{data.frame}s if
#'   multiple inputs are given.
#' @seealso \code{\link[taxize]{ncbi_getbyid}}, \code{\link[taxize]{ncbi_getbyname}}
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}, Zachary Foster
#'   \email{zacharyfoster1989@@gmail.com}
#' @rdname ncbi_search-deprecated
#' @examples \donttest{
#' # A single species
#' out <- ncbi_search(taxa="Umbra limi", seqrange = "1:2000")
#' # Get the same species information using a taxonomy id
#' out <- ncbi_search(id = "75935", seqrange = "1:2000")
#' # If the taxon name is unique, using the taxon name and id are equivalent
#' all(ncbi_search(id = "75935") ==  ncbi_search(taxa="Umbra limi"))
#' # If the taxon name is not unique, use taxon id
#' #  "266948" is the uid for the butterfly genus, but there is also a genus of orchids with the
#' #  same name
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
#'
#' # Using the getrelated and entrez_query options
#' ncbi_search(taxa = "Olpidiopsidales", limit = 5, getrelated = TRUE,
#'             entrez_query = "18S[title] AND 28S[title]")
#' }
#' @export
ncbi_search <- function(taxa = NULL, id = NULL, seqrange="1:3000", getrelated=FALSE, limit = 500,
                        entrez_query = NULL, hypothetical = FALSE, verbose=TRUE)
{
  .Deprecated(msg="This function is deprecated - will be removed in a future version of this pacakge. See ?`taxize-deprecated`")

  # Function to search for sequences with esearch --------------------------------------------------
  search_for_sequences <- function(id) {
    if (is.na(id)) return(NULL)
    # Construct search query  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    query_term <- paste0("xXarbitraryXx[porgn:__txid", id, "] AND ", seqrange, " [SLEN]")
    if (!is.null(entrez_query)) query_term <- paste(query_term, entrez_query, sep = " AND ")
    query <- list(db = "nuccore", retmax = limit, term = query_term)
    # Submit query to NCBI - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    query_init <- GET(url_esearch, query=query)
    stop_for_status(query_init)
    # Parse result - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    esearch_result <- xpathApply(content(query_init, as="parsed"), "//eSearchResult")[[1]]
    if (as.numeric(xmlValue(xpathApply(esearch_result, "//Count")[[1]])) == 0) {
      return(NULL)
    } else {
      return(xpathSApply(esearch_result, "//IdList//Id", xmlValue)) # a list of sequence ids
    }
  }

  # Function to parse results from http query ------------------------------------------------------
  parseres <- function(x){
    outsum <- xpathApply(content(x, as="parsed"), "//eSummaryResult")[[1]]
    names <- sapply(getNodeSet(outsum[[1]], "//Item"), xmlGetAttr, name="Name") # gets names of values in summary
    predicted <- as.character(sapply(getNodeSet(outsum, "//Item"), xmlValue)[str_detect(names, "Caption")]) #  get access numbers
    has_access_prefix <- grepl("_", predicted)
    access_prefix <- unlist(Map(function(x, y) ifelse(x, strsplit(y, "_")[[1]][[1]], NA),
                                has_access_prefix, predicted))
    predicted[has_access_prefix] <- vapply(strsplit(predicted[has_access_prefix], "_"), `[[`, character(1), 2)

    length_ <- as.numeric(sapply(getNodeSet(outsum, "//Item"), xmlValue)[str_detect(names, "Length")]) # gets seq lengths
    gis <- as.numeric(sapply(getNodeSet(outsum, "//Item"), xmlValue)[str_detect(names, "Gi")]) # gets GI numbers
    spnames <- sapply(getNodeSet(outsum, "//Item"), xmlValue)[str_detect(names, "Title")] # gets seq lengths # get spp names
    spused <- sapply(spnames, function(x) paste(str_split(x, " ")[[1]][1:2], sep="", collapse=" "), USE.NAMES=FALSE)
    genesavail <- sapply(spnames, function(x) paste(str_split(x, " ")[[1]][-c(1:2)], sep="", collapse=" "), USE.NAMES=FALSE)
    df <- data.frame(spused=spused, length=length_, genesavail=genesavail, access_num=predicted, ids=gis, stringsAsFactors=FALSE)
    if (!hypothetical) df <- df[!(access_prefix %in% c("XM","XR")),]
    return(df)
  }

  # Function to download and parse sequence summary information using esummary ---------------------
  download_summary <- function(seq_id) {
    actualnum <- length(seq_id)
    if(actualnum > 10000){
      q <- list(db = "nucleotide")
      getstart <- seq(from=1, to=actualnum, by=10000)
      getnum <- c(rep(10000, length(getstart)-1), actualnum-sum(rep(10000, length(getstart)-1)))
      iterlist = list()
      for(i in seq_along(getstart)){
        q$id = paste(seq_id[getstart[i]:(getstart[i]+(getnum[i]-1))], collapse=" ")
        q$retstart <- getstart[i]
        q$retmax <- getnum[i]
        query_res <- POST(url_esummary, body=q)
        stop_for_status(query_res)
        iterlist[[i]] <- parseres(query_res)
      }
      df <- data.frame(rbindlist(iterlist))
    } else
    {
      q <- list(db = "nucleotide", id = paste(seq_id, collapse=" "))
      query_res <- POST(url_esummary, body=q)
      stop_for_status(query_res)
      df <- parseres(query_res)
    }
    return(df)
  }

  # Function to get a taxon's parent ---------------------------------------------------------------
  get_parent <- function(id) {
    if (!is.na(id)) {
      ancestry <- classification(id = id, db = "ncbi")[[1]]
      if (nrow(ancestry) > 1) {
        parent_name <- ancestry$name[nrow(ancestry) - 1]
        return(get_uid(parent_name, verbose = verbose))
      }
    }
    if (!is.null(names(id)) && grepl(" ", names(id))) { #if a name is given and looks like a species
      parent_name <- strsplit(names(id), " ")[[1]][[1]]
      return(get_uid(parent_name, verbose = verbose))
    }
    return(NA)
  }

  # Function to process queries one at a time ------------------------------------------------------
  foo <- function(xx) {
    # Search for sequence IDs for the given taxon  - - - - - - - - - - - - - - - - - - - - - - - - -
    mssg(verbose, paste("Working on ", names(xx), "...", sep=""))
    mssg(verbose, "...retrieving sequence IDs...")
    seq_ids <- search_for_sequences(xx)
    # Search for sequences of the taxons parent if necessary and possible  - - - - - - - - - - - - -
    if (is.null(seq_ids) && getrelated) {
      mssg(verbose, paste("no sequences for ", names(xx), " - getting other related taxa", sep=""))
      parent_id <- get_parent(xx)
      if (is.na(parent_id)) {
        mssg(verbose, paste0("no related taxa found"))
      } else {
        mssg(verbose, paste0("...retrieving sequence IDs for ", names(xx), "..."))
        seq_ids <- search_for_sequences(parent_id)
      }
    }
    # Retrieve sequence information  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if (is.null(seq_ids)) {
      mssg(verbose, "no sequences found")
      df <- data.frame(character(0), numeric(0), character(0), character(0), numeric(0))
    } else {
      mssg(verbose, "...retrieving available genes and their lengths...")
      df <- download_summary(seq_ids)
      mssg(verbose, "...done.")
    }
    # Format output  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    names(df) <- c("taxon","length","gene_desc","acc_no","gi_no")
    return(df)
  }

  # Constants --------------------------------------------------------------------------------------
  url_esearch <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
  url_esummary <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi"

  # Argument validation ----------------------------------------------------------------------------
  if (sum(c(is.null(taxa), is.null(id))) != 1) {
    stop("Either taxa or id must be specified, but not both")
  }

  # Convert 'taxa' to 'id' if 'taxa' is supplied ---------------------------------------------------
  if (is.null(id)) {
    id <- get_uid(taxa, verbose=verbose)
    names(id) <- taxa
  } else {
    id <- as.character(id)
    class(id) <- "uid"
    names(id) <- id
  }

  # look up sequences for taxa ids -----------------------------------------------------------------
  foo_safe <- plyr::failwith(NULL, foo)
  if (length(id) == 1) {
    foo_safe(id)
  } else {
    lapply(id, foo_safe)
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
  .Defunct("ncbi_search", "taxize", "Function name changed. See ncbi_search")
}
