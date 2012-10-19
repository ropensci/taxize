\name{itis_get_name}
\alias{itis_get_name}
\title{Get family names to make Phylomatic input object.}
\usage{
  itis_get_name(query = NULL, get = NULL)
}
\arguments{
  \item{x}{Quoted tsn number (taxonomic serial number).}
}
\value{
  Family name for the searched taxon.
}
\description{
  Get family names to make Phylomatic input object.
}
\examples{
\dontrun{
itis_get_name(query="Helianthus annuus", get="family")
}
}

