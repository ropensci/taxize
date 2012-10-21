\name{phylomatic_tree}
\alias{phylomatic_tree}
\title{Format tree string, submit to Phylomatic, get newick tree.}
\usage{
  phylomatic_tree(x, get, format, retphylo = TRUE,
    url = "http://phylodiversity.net/phylomatic/pm/phylomatic.cgi")
}
\arguments{
  \item{x}{Phylomatic format input.}

  \item{get}{'GET' or 'POST' format for submission to the
  website.}

  \item{format}{Newick (new) or xml (xml) output.}

  \item{retphylo}{Return phylo tree object (TRUE or
  FALSE).}

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
  Submitted in POST format (not GET format).  Version:
  already have in Phylomatic input format forward slash (/
  -> %2F) newline (\n -> %0D%0A)
}
\examples{
\dontrun{
dat_ <- laply(list("36616", "19322", "183327"), itis_phymat_format, format='rsubmit')
tree <- phylomatic_tree(dat_, 'GET', 'new', 'TRUE')
plot(tree)
}
}

