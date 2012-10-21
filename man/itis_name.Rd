\name{itis_name}
\alias{itis_name}
\title{Get family names to make Phylomatic input object.}
\usage{
  itis_name(query = NULL, get = NULL)
}
\arguments{
  \item{query}{TSN number (taxonomic serial number).}

  \item{get}{The rank of the taxonomic name to get. See
  data(rank_ref) for possible names.}
}
\value{
  Taxonomic name for the searched taxon.
}
\description{
  Get family names to make Phylomatic input object.
}
\examples{
\dontrun{
itis_name(query="Helianthus annuus", get="family")
}
}

