\name{rbladj}
\alias{rbladj}
\title{Run Phylocom's bladj from R}
\usage{
rbladj(tree = NULL, ages = NULL, path = NULL, fixroot = FALSE)
}
\arguments{
  \item{tree}{(phylo/character) If left NULL, it is
  expected that you already have a phylo file with your
  newick tree in your directory with your phylocom
  executable}

  \item{ages}{(data.frame) If left NULL, it is expected
  that you already have an ages file with your node names
  and ages in your directory with your Phylocom executable}

  \item{path}{(character) Path to the folder with at least
  the Phylocom executable}

  \item{fixroot}{(logical) If TRUE root name is changed to
  the oldest root in your ages file. If FALSE (default),}
}
\description{
Run Phylocom's bladj from R
}
\examples{
\dontrun{
taxa <- names_list("species", 15)
tree <- phylomatic_tree(taxa=taxa, get = 'POST')
# set path to where your dir is with phylocom executable, and optionally phylo
# and/or ages files
path = "path to phylocom executable directory"
g <- rbladj(tree, path=path, fixroot=TRUE)
g$edge.length
plot(g)
}
}

