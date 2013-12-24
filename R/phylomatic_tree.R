#' Query Phylomatic for a phylogenetic tree.
#' 
#' @import httr ape
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
#' @details Use the web interface here http://phylodiversity.net/phylomatic/
#' @return Newick formatted tree or nexml text.
#' @examples \dontrun{ 
#' # Input taxonomic names
#' taxa <- c("Poa annua", "Phlox diffusa", "Helianthus annuus")
#' tree <- phylomatic_tree(taxa=taxa, get = 'POST')
#' plot(tree)
#' 
#' # Genus names
#' taxa <- c("Poa", "Phlox", "Helianthus")
#' tree <- phylomatic_tree(taxa=taxa, storedtree='R20120829', get='POST')
#' plot(tree)
#' 
#' # Lots of names
#' taxa <- c("Poa annua", "Collomia grandiflora", "Lilium lankongense", "Phlox diffusa", 
#' "Iteadaphne caudata", "Gagea sarmentosa", "Helianthus annuus")
#' tree <- phylomatic_tree(taxa=taxa, get = 'POST')
#' plot(tree, no.margin=TRUE)
#' 
#' # In parallel with parallel=TRUE speeds up dramatically by doing the 
#' # gathering of family names from NCBI in parallel
#' registerDoMC(cores=4)
#' phylomatic_tree(taxa=taxa, get='POST', parallel=TRUE)
#'    
#' # Output NeXML format
#' taxa <- c("Gonocarpus leptothecus", "Gonocarpus leptothecus", "Lilium lankongense")
#' out <- phylomatic_tree(taxa=taxa, get = 'POST', outformat = "nexml")
#' out
#' cat(out)
#' }
#' @export

phylomatic_tree <- function(taxa, taxnames = TRUE, get = 'GET',
  informat = "newick", method = "phylomatic", storedtree = "smith2011", 
  taxaformat = "slashpath", outformat = "newick", clean = "true", 
  parallel=FALSE)
{
  url = "http://phylodiversity.net/phylomatic/pmws"
  
  if(taxnames){
    if(parallel){ 
      dat_ <- llply(taxa, itis_phymat_format, format='isubmit', .parallel=TRUE)
    } else {
      dat_ <- llply(taxa, itis_phymat_format, format='isubmit')
    }
    
    checknas <- sapply(dat_, function(x) strsplit(x, "/")[[1]][1])
    checknas2 <- checknas[grep("na", checknas)]
    if(length(checknas2)>0)
      stop(sprintf("A family was not found for the following taxa:\n %s \n\n try setting taxnmaes=FALSE, and passing in a vector of strings, like \n%s", 
                   paste(sapply(dat_, function(x) strsplit(x, "/")[[1]][3])[grep("na", checknas)], collapse=", "),
                   'phylomatic_tree(taxa = c("asteraceae/taraxacum/taraxacum_officinale", "ericaceae/gaylussacia/gaylussacia_baccata", "ericaceae/vaccinium/vaccinium_pallidum"), taxnames=FALSE, parallel=FALSE)'
      ))
    
  } else { dat_ <- taxa }
  
  if (length(dat_) > 1) { dat_ <- paste(dat_, collapse = "\n") } else { dat_ <- dat_ }
  
  args <- compact(list(taxa = dat_, informat = informat, method = method, 
                       storedtree = storedtree, taxaformat = taxaformat, 
                       outformat = outformat, clean = clean))
  
  if (get == 'POST') {  
    tt <- POST(url, query=args)
  } else if (get == 'GET') {
    tt <- GET(url, query=args)
  } else
  { stop("Error: get must be one of 'POST' or 'GET'") }
  stop_for_status(tt)
  out <- content(tt, as="text")
  
  outformat <- match.arg(outformat, choices=c("nexml",'newick'))
  getnewick <- function(x){
    tree <- gsub("\n", "", x[[1]])
    read.tree(text = colldouble(tree))
  }
  
  switch(outformat, 
         nexml = out,
         newick = getnewick(out))
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