#' Query Phylomatic for a phylogenetic tree.
#' 
#' @import httr ape
#' @importFrom stringr str_extract
#' @param taxa Phylomatic format input of taxa names.
#' @param taxnames If true, we get the family names for you to attach to your 
#'    species names to send to Phylomatic API. If FALSE, you have to provide the 
#'    strings in the right format.
#' @param get 'GET' or 'POST' format for submission to the website.
#' @param informat One of newick, nexml, or cdaordf. If using a stored tree, 
#'    informat should always be newick.
#' @param method One of phylomatic or convert
#' @param storedtree One of R20120829 (Phylomatic tree R20120829 for plants), 
#'    smith2011 (Smith 2011, plants), or binindaemonds2007 (Bininda-Emonds 2007, 
#'    mammals).
#' @param taxaformat Only option is slashpath for now. Leave as is.
#' @param outformat One of newick, nexml, or fyt.
#' @param clean Return a clean tree or not.
#' @param parallel Run in parallel or not.
#' @param verbose Print messages (default: TRUE).
#' @details Use the web interface here http://phylodiversity.net/phylomatic/
#' @return Newick formatted tree or nexml text.
#' @examples \dontrun{ 
#' # Input taxonomic names
#' taxa <- c("Poa annua", "Phlox diffusa", "Helianthus annuus")
#' tree <- phylomatic_tree(taxa=taxa, get = 'POST')
#' plot(tree, no.margin=TRUE)
#' 
#' # Genus names
#' taxa <- c("Poa", "Phlox", "Helianthus")
#' tree <- phylomatic_tree(taxa=taxa, storedtree='R20120829', get='POST')
#' plot(tree, no.margin=TRUE)
#' 
#' # Lots of names
#' taxa <- c("Poa annua", "Collomia grandiflora", "Lilium lankongense", "Phlox diffusa", 
#' "Iteadaphne caudata", "Gagea sarmentosa", "Helianthus annuus")
#' tree <- phylomatic_tree(taxa=taxa, get = 'POST')
#' plot(tree, no.margin=TRUE)
#'    
#' # Output NeXML format
#' taxa <- c("Gonocarpus leptothecus", "Gonocarpus leptothecus", "Lilium lankongense")
#' out <- phylomatic_tree(taxa=taxa, get = 'POST', outformat = "nexml")
#' out
#' cat(out)
#' }
#' @export

phylomatic_tree <- function(taxa, taxnames = TRUE, get = 'GET',
  informat = "newick", method = "phylomatic", storedtree = "R20120829", 
  taxaformat = "slashpath", outformat = "newick", clean = "true", db="apg", verbose=TRUE)
{
  url = "http://phylodiversity.net/phylomatic/pmws"
  
  if(taxnames){
    dat_ <- phylomatic_format(taxa, format='isubmit', db=db)
    
    checknas <- sapply(dat_, function(x) strsplit(x, "/")[[1]][1])
    checknas2 <- checknas[match("na", checknas)]
    if(is.numeric(checknas2))
      stop(sprintf("A family was not found for the following taxa:\n %s \n\n try setting taxnmaes=FALSE, and passing in a vector of strings, like \n%s", 
                   paste(sapply(dat_, function(x) strsplit(x, "/")[[1]][3])[match("na", checknas)], collapse=", "),
                   'phylomatic_tree(taxa = c("asteraceae/taraxacum/taraxacum_officinale", "ericaceae/gaylussacia/gaylussacia_baccata", "ericaceae/vaccinium/vaccinium_pallidum"), taxnames=FALSE, parallel=FALSE)'
      ))
    
  } else { dat_ <- taxa }
  
  if (length(dat_) > 1) { dat_ <- paste(dat_, collapse = "\n") } else { dat_ <- dat_ }
  
  args <- compact(list(taxa = dat_, informat = informat, method = method, 
                       storedtree = storedtree, taxaformat = taxaformat, 
                       outformat = outformat, clean = clean))
  
  if (get == 'POST') {  
#     tt <- POST(url, body=list(taxa=dat_), query=args, multipart=FALSE)
    out <- postForm(url, .params=args, style = "POST")
  } else if (get == 'GET') {
    tt <- GET(url, query=args)
    stop_for_status(tt)
    out <- content(tt, as="text")
  } else
  { stop("Error: get must be one of 'POST' or 'GET'") }

  if(grepl("No taxa in common", out)){
    stop(out)
  } else
  {
    # parse out missing taxa note
    if(grepl("\\[NOTE: ", out)){
      taxa_na <- str_extract(out, "NOTE:.+")
      taxa_na2 <- str_extract(taxa_na, ":\\s[A-Za-z].+")
      taxa_na2 <- strsplit(taxa_na2, ",")[[1]][-length(strsplit(taxa_na2, ",")[[1]])]
      taxa_na2 <- gsub(":|\\s", "", taxa_na2)
      taxa_na2 <- sapply(taxa_na2, function(x) strsplit(x, "/")[[1]][[3]], USE.NAMES=FALSE)
      taxa_na2 <- taxize_capwords(gsub("_", " ", taxa_na2), onlyfirst=TRUE)
      
      mssg(verbose, taxa_na)
      out <- gsub("\\[NOTE:.+", ";\n", out)
    }
    
    outformat <- match.arg(outformat, choices=c("nexml",'newick'))
    getnewick <- function(x){
      tree <- gsub("\n", "", x[[1]])
      read.tree(text = colldouble(tree))
    }
    
    res <- switch(outformat, 
           nexml = out,
           newick = getnewick(out))
    class(res) <- c("phylo","phylomatic")
    attr(res, "missing") <- taxa_na2
    return( res )
  }
}

collapse_double_root <- function(y) {
  temp <- str_split(y, ")")[[1]]
  double <- c(length(temp)-1, length(temp))
  tempsplit <- temp[double]
  tempsplit_1 <- str_split(tempsplit[1], ":")[[1]][2]
  tempsplit_2 <-str_split(tempsplit[2], ":")[[1]]
  rootlength <- as.numeric(tempsplit_1) + 
    as.numeric(str_split(tempsplit_2[2], ";")[[1]][1])
  newx <- paste(")", tempsplit_2[1], ":", rootlength, ";", sep="")
  newpre <- str_replace(temp[1], "[(]", "")
  allelse <- temp[-1]
  allelse <- allelse[setdiff(1:length(allelse), double-1)]
  allelse <- paste(")", allelse, sep="")
  tempdone <- paste(newpre, paste(allelse, collapse=""), newx, sep="")
  return(tempdone)
}

colldouble <- function(z) {
  if ( class ( try ( read.tree(text = z), silent = T ) ) %in% 'try-error') 
  { treephylo <- collapse_double_root(z) } else
  { treephylo <- z }
  return(treephylo)
}