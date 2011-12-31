\name{get_phylomatic_tree}
\alias{get_phylomatic_tree}
\title{Format tree string, submit to Phylomatic, get newick tree.}
\usage{
  get_phylomatic_tree(x, convert = TRUE, get, format,
    retphylo = TRUE,
    url = "http://phylodiversity.net/phylomatic/pm/phylomatic.cgi")
}
\arguments{
  \item{x}{Phylomatic format input.}

  \item{convert}{One of 'TRUE' of 'FALSE'.}

  \item{get}{'GET' or 'POST' format for submission to the
  website.}

  \item{format}{Newick (new) or xml (xml) output.}

  \item{retphylo}{Return phylo tree object (TRUE or
  FALSE).}
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
dat_ <- laply(list("36616", "19322", "183327"), get_phymat_format,
      format='rsubmit', .progress="text")
dat_mine <- paste(dat_, collapse="\%0D\%0A") # collapse and replace \\n's
tree <- get_phylomatic_tree(dat_mine, 'FALSE', 'GET', 'new', 'TRUE')
plot(tree)
}
}

