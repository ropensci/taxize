#' Format tree string, submit to Phylomatic, get newick tree.
#' 
#' @import httr ape
#' @param taxa Phylomatic format input of taxa names.
#' @param get 'GET' or 'POST' format for submission to the website.
#' @param informat One of newick, nexml, or cdaordf. If using a stored tree, informat
#' 		should always be newick.
#' @param method One of phylomatic or convert
#' @param storedtree One of R20120829 (Phylomatic tree R20120829 for plants), 
#' 		smith2011 (Smith 2011, plants), or binindaemonds2007 (Bininda-Emonds 2007, mammals).
#' @param taxaformat Only option is slashpath for now. Leave as is.
#' @param outformat One of newick, nexml, or fyt.
#' @param url The base URL for the Phylomatic API service, leave as is.
#' @details Use the web interface here \link{http://phylodiversity.net/phylomatic/}
#' @return Newick formatted tree.
#' @examples \dontrun{
#' # Getting taxonomic information from ITIS, if you already have taxonomic serial numbers (TSNs)
#' dat_ <- laply(list("36616", "19322", "183327", "36616", "41107", "181835", "25929"), itis_phymat_format, format='isubmit')
#' tree <- phylomatic_tree(taxa=taxa, get = 'POST', informat='newick', method = "phylomatic", 
#' 		storedtree = "R20120829", taxaformat = "slashpath", outformat = "newick", clean = "true")
#' plot(tree)
#' 
#' # Input taxonomic names
#' taxa <- c("Poa annua", "Abies procera", "Helianthus annuus")
#' tree <- phylomatic_tree(taxa=taxa, get = 'POST', informat='newick', method = "phylomatic", 
#' 		storedtree = "R20120829", taxaformat = "slashpath", outformat = "newick", clean = "true")
#' plot(tree)
#' 
#' # Lots of names
#' taxa <- c("Poa annua", "Abies procera", "Helianthus annuus", "Collomia grandiflora", 
#' 		"Ribes latifolium", "Arctostaphylos manzanita", "Phlox glabriflora", "Phlox diffusa", 
#' 		"Datura wrightii", "Nicotiana glauca", "Nicotiana tomentosa", "Mimulus bicolor")
#' tree <- phylomatic_tree(taxa=taxa, get = 'POST', informat='newick', method = "phylomatic", 
#' 		storedtree = "R20120829", taxaformat = "slashpath", outformat = "newick", clean = "true")
#' plot(tree, no.margin=T)
#' }
#' @export
phylomatic_tree <- function (taxa, taxnames = TRUE, get = 'GET', informat = "newick", method = "phylomatic", 
	storedtree = "R20120829", taxaformat = "slashpath", outformat = "newick", 
	clean = "true", url = "http://phylodiversity.net/phylomatic/pmws")
{
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
  
  if(taxnames){
  	tsns <- get_tsn(taxa)
  	dat_ <- laply(tsns, itis_phymat_format, format='isubmit')
  } else
  	{ dat_ <- taxa }
  if (length(dat_) > 1) { dat_ <- paste(dat_, collapse = "\n") } else { dat_ <- dat_ }
  
  args <- compact(list(taxa = dat_, informat = informat, method = method, 
  										 storedtree = storedtree, taxaformat = taxaformat, 
  										 outformat = outformat, clean = clean))
  
  if (get == 'POST') {  
  	tt <- content(POST(url, query=args))
  	tree <- gsub("\n", "", tt[[1]])
  } else if (get == 'GET') {
    	tt <- content(GET(url, query=args))
      tree <- gsub("\n", "", tt[[1]])
  } else
  	{ stop("Error: get must be one of 'POST' or 'GET'") }
  
  if (outformat == 'nexml') {
  	stop("Can't currently parse nexml trees...")
  } else
  	if (outformat == 'newick') {
  		return( read.tree(text = colldouble(tree)) )
  	}
  		else
  		{ stop("Error: outformat must be one of 'newick' or 'newxml'") }
}