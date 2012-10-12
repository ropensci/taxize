\name{itis_get_phymat_format}
\alias{itis_get_phymat_format}
\title{Get family names to make Phylomatic input object, and output input string
   to Phylomatic for use in the function get_phylomatic_tree.}
\usage{
  itis_get_phymat_format(tsn = NA, format)
}
\arguments{
  \item{tsn}{quoted tsn number (taxonomic serial number)}

  \item{format}{output format, isubmit (you can paste in to
  the Phylomatic}
}
\value{
  e.g., "pinaceae/pinus/pinus_contorta", in Phylomatic
  submission format.
}
\description{
  Get family names to make Phylomatic input object, and
  output input string to Phylomatic for use in the function
  get_phylomatic_tree.
}
\examples{
\dontrun{
dat_ <- laply(list("36616", "19322", "183327"), itis_get_phymat_format,
      format='rsubmit', .progress="text")
dat_mine <- paste(dat_, collapse="\%0D\%0A") # collapse and replace \\n's
tree <- get_phylomatic_tree(dat_mine, 'FALSE', 'GET', 'new', 'TRUE')
plot(tree)
}
}

