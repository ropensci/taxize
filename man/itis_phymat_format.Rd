\name{itis_phymat_format}
\alias{itis_phymat_format}
\title{Get family names to make Phylomatic input object, and output input string
   to Phylomatic for use in the function phylomatic_tree.}
\usage{
  itis_phymat_format(taxa = NA, format = "isubmit",
    db = "ncbi", locally = FALSE, cn = NULL)
}
\arguments{
  \item{taxa}{quoted tsn number (taxonomic serial number)}

  \item{format}{output format, isubmit (you can paste in to
  the Phylomatic website), or 'rsubmit' to use in fxn
  phylomatic_tree}

  \item{db}{One of ncbi or itis}

  \item{locally}{If TRUE, queries are run locally in
  sqlite3; if FALSE (the default), queries are run against
  the ITIS web API. locally=TRUE should be faster in almost
  all cases.}

  \item{cn}{sqlite3 connection object}
}
\value{
  e.g., "pinaceae/pinus/pinus_contorta", in Phylomatic
  submission format.
}
\description{
  Get family names to make Phylomatic input object, and
  output input string to Phylomatic for use in the function
  phylomatic_tree.
}
\examples{
\dontrun{
laply(c("Poa annua", "Abies procera", "Helianthus annuus"), itis_phymat_format, format='rsubmit')
}
}

