\name{phylomatic_tree}
\alias{phylomatic_tree}
\title{Query Phylomatic for a phylogenetic tree.}
\usage{
phylomatic_tree(taxa, taxnames = TRUE, get = "GET", informat = "newick",
  method = "phylomatic", storedtree = "smith2011",
  taxaformat = "slashpath", outformat = "newick", clean = "true",
  parallel = FALSE)
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
Newick formatted tree or nexml text.
}
\description{
Query Phylomatic for a phylogenetic tree.
}
\details{
Use the web interface here
http://phylodiversity.net/phylomatic/
}
\examples{
\dontrun{
# Input taxonomic names
taxa <- c("Poa annua", "Phlox diffusa", "Helianthus annuus")
tree <- phylomatic_tree(taxa=taxa, get = 'POST')
plot(tree)

# Lots of names
taxa <- c("Poa annua", "Collomia grandiflora", "Lilium lankongense", "Phlox diffusa",
"Iteadaphne caudata", "Gagea sarmentosa", "Helianthus annuus")
tree <- phylomatic_tree(taxa=taxa, get = 'POST')
plot(tree, no.margin=TRUE)

# In parallel with parallel=TRUE speeds up dramatically by doing the
gathering of
# family names from NCBI in parallel
registerDoMC(cores=4)
phylomatic_tree(taxa=taxa, get='POST', parallel=TRUE)

# Output NeXML format
taxa <- c("Gonocarpus leptothecus", "Gonocarpus leptothecus", "Lilium lankongense")
out <- phylomatic_tree(taxa=taxa, get = 'POST', outformat = "nexml")
out
cat(out)
}
}

