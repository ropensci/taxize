\name{phylomatic_tree}
\alias{phylomatic_tree}
\title{Format tree string, submit to Phylomatic, get newick tree.}
\usage{
  phylomatic_tree(taxa, taxnames = TRUE, get = "GET",
    informat = "newick", method = "phylomatic",
    storedtree = "R20120829", taxaformat = "slashpath",
    outformat = "newick", clean = "true", parallel = TRUE)
}
\arguments{
  \item{taxa}{Phylomatic format input of taxa names.}

  \item{taxnames}{If true, we get the family names for you
  to attach to your species names to send to Phylomatic
  API. If FALSE, you have to provide the strings in the
  right format.}

  \item{get}{'GET' or 'POST' format for submission to the
  website.}

  \item{informat}{One of newick, nexml, or cdaordf. If
  using a stored tree, informat should always be newick.}

  \item{method}{One of phylomatic or convert}

  \item{storedtree}{One of R20120829 (Phylomatic tree
  R20120829 for plants), smith2011 (Smith 2011, plants), or
  binindaemonds2007 (Bininda-Emonds 2007, mammals).}

  \item{taxaformat}{Only option is slashpath for now. Leave
  as is.}

  \item{outformat}{One of newick, nexml, or fyt.}

  \item{clean}{Return a clean tree or not.}

  \item{parallel}{Run in parallel or not.}
}
\value{
  Newick formatted tree.
}
\description{
  Format tree string, submit to Phylomatic, get newick
  tree.
}
\details{
  Use the web interface here
  http://phylodiversity.net/phylomatic/
}
\examples{
\dontrun{
# Input taxonomic names
taxa <- c("Poa annua", "Abies procera", "Helianthus annuus")
tree <- phylomatic_tree(taxa=taxa, get = 'POST', informat='newick', method = "phylomatic",
		storedtree = "R20120829", taxaformat = "slashpath", outformat = "newick", clean = "true")
plot(tree)

# Lots of names
taxa <- c("Poa annua", "Abies procera", "Helianthus annuus", "Collomia grandiflora",
		"Ribes latifolium", "Arctostaphylos manzanita", "Phlox glabriflora", "Phlox diffusa",
		"Datura wrightii", "Nicotiana glauca", "Nicotiana tomentosa", "Mimulus bicolor")
tree <- phylomatic_tree(taxa=taxa, get = 'POST', informat='newick', method = "phylomatic",
		storedtree = "R20120829", taxaformat = "slashpath", outformat = "newick", clean = "true")
plot(tree, no.margin=T)

# In parallel with parallel=TRUE speeds up dramatically by doing the gathering of
# family names from NCBI in parallel
registerDoMC(cores=4)  # I have a mac, so using doMC package, use whatever you like
phylomatic_tree(taxa=taxa, get = 'POST', informat='newick', method = "phylomatic",
		storedtree = "R20120829", taxaformat = "slashpath", outformat = "newick", clean = "true", parallel=FALSE)
}
}

