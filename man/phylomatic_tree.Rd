\name{phylomatic_tree}
\alias{phylomatic_tree}
\title{Format tree string, submit to Phylomatic, get newick tree.}
\usage{
  phylomatic_tree(taxa, taxnames = TRUE, get = "GET",
    informat = "newick", method = "phylomatic",
    storedtree = "R20120829", taxaformat = "slashpath",
    outformat = "newick", clean = "true",
    url = "http://phylodiversity.net/phylomatic/pmws")
}
\arguments{
  \item{taxa}{Phylomatic format input of taxa names.}

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

  \item{url}{The base URL for the Phylomatic API service,
  leave as is.}
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
  \link{http://phylodiversity.net/phylomatic/}
}
\examples{
\dontrun{
# Getting taxonomic information from ITIS, if you already have taxonomic serial numbers (TSNs)
dat_ <- laply(list("36616", "19322", "183327", "36616", "41107", "181835", "25929"), itis_phymat_format, format='isubmit')
tree <- phylomatic_tree(taxa=taxa, get = 'POST', informat='newick', method = "phylomatic",
		storedtree = "R20120829", taxaformat = "slashpath", outformat = "newick", clean = "true")
plot(tree)

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
}
}

