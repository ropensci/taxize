\name{phylomatic_tree2}
\alias{phylomatic_tree2}
\title{Format tree string, submit to Phylomatic, get newick or nexml tree.}
\usage{
phylomatic_tree2(taxa, get = "GET", informat = "newick",
  method = "phylomatic", storedtree = "smith2011", outformat = "newick",
  clean = "true")
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

  \item{outformat}{One of newick, nexml, or fyt.}

  \item{clean}{Return a clean tree or not.}
}
\value{
Newick formatted tree, or nexml xml object.
}
\description{
Newer input format to the API, doesn't work that well. Do
use phylomatic_tree()
}
\details{
Use the web interface here
http://phylodiversity.net/phylomatic/
}
\examples{
\dontrun{
# Input taxonomic names
taxa <- c("Collomia grandiflora", "Lilium lankongense", "Helianthus annuus")
tree <- phylomatic_tree2(taxa=taxa, get = 'POST', informat='newick',
   method = "phylomatic", storedtree = "smith2011",
   outformat = "newick", clean = "true")
plot(tree)

# Lots of names
taxa <- c("Collomia grandiflora", "Lilium lankongense", "Phlox diffusa",
          "Iteadaphne caudata", "Nicotiana tomentosa", "Gagea sarmentosa")
tree <- phylomatic_tree2(taxa=taxa, get = 'POST', informat='newick',
                        method = "phylomatic", storedtree = "smith2011",
                        outformat = "newick", clean = "true")
plot(tree)

# Output NeXML format
taxa <- c("Gonocarpus leptothecus", "Gonocarpus leptothecus", "Impatiens davidis")
out <- phylomatic_tree2(taxa=taxa, get = 'POST', informat='newick', method = "phylomatic",
   storedtree = "smith2011", outformat = "nexml", clean = "true")
library(RNeXML)
read.nexml(out, type="nexml", asText=TRUE)
}
}

