#' Format tree string, submit to Phylomatic, get newick or nexml tree.
#' 
#' Newer input format to the API, doesn't work that well. Do use phylomatic_tree()
#' 
#' @import httr ape
#' @param taxa Phylomatic format input of taxa names.
#' @param get 'GET' or 'POST' format for submission to the website.
#' @param informat One of newick, nexml, or cdaordf. If using a stored tree, 
#'    informat should always be newick.
#' @param method One of phylomatic or convert
#' @param storedtree One of R20120829 (Phylomatic tree R20120829 for plants), 
#' 		smith2011 (Smith 2011, plants), or binindaemonds2007 (Bininda-Emonds 2007, 
#'   	mammals).
#' @param outformat One of newick, nexml, or fyt.
#' @param clean Return a clean tree or not.
#' @details Use the web interface here http://phylodiversity.net/phylomatic/
#' @return Newick formatted tree, or nexml xml object.
#' @noRd
#' @examples \donttest{ 
#' # Input taxonomic names
#' taxa <- c("Collomia grandiflora", "Lilium lankongense", "Helianthus annuus")
#' tree <- phylomatic_tree2(taxa=taxa, get = 'POST', informat='newick', 
#'    method = "phylomatic", storedtree = "smith2011", 
#'    outformat = "newick", clean = "true")
#' plot(tree)
#' 
#' # Lots of names
#' taxa <- c("Collomia grandiflora", "Lilium lankongense", "Phlox diffusa", 
#'           "Iteadaphne caudata", "Nicotiana tomentosa", "Gagea sarmentosa")
#' tree <- phylomatic_tree2(taxa=taxa, get = 'POST', informat='newick', 
#'                         method = "phylomatic", storedtree = "smith2011",
#'                         outformat = "newick", clean = "true")
#' plot(tree)
#' 
#' # Output NeXML format
#' taxa <- c("Gonocarpus leptothecus", "Gonocarpus leptothecus", "Impatiens davidis")
#' out <- phylomatic_tree2(taxa=taxa, get = 'POST', informat='newick', method = "phylomatic", 
#'    storedtree = "smith2011", outformat = "nexml", clean = "true")
#' library(RNeXML)
#' read.nexml(out, type="nexml", asText=TRUE)
#' }

phylomatic_tree2 <- function(taxa, get = 'GET', informat = "newick", method = "phylomatic", 
  storedtree = "smith2011", outformat = "newick", clean = "true")
{
  url <- "http://phylodiversity.net/phylomatic/pmws"
  taxa <- sapply(taxa, function(x) gsub("\\s", "_", x), USE.NAMES=FALSE)
  if (length(taxa) > 1) { taxa <- paste(taxa, collapse = "\n") } else { taxa <- taxa }  
  args <- compact(list(taxa = taxa, informat = informat, method = method, 
                       storedtree = storedtree, outformat = outformat, clean = clean))
  get <- match.arg(get, choices=c("GET",'POST'))
  out <- eval(parse(text=get))(url, query=args)
  stop_for_status(out)
  tt <- content(out, as="text")
  
  outformat <- match.arg(outformat, choices=c("nexml",'newick'))
  getnewick <- function(x){
    tree <- gsub("\n", "", x[[1]])
    read.tree(text = colldouble(tree))
  }

  switch(outformat, 
         nexml = tt,
         newick = getnewick(tt))
}