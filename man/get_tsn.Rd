\name{get_tsn}
\alias{get_tsn}
\title{Get the TSN code for a search term.}
\usage{
  get_tsn(searchterm, searchtype)
}
\arguments{
  \item{searchterm}{Any common or scientific name.}

  \item{searchtype}{One of 'sciname', 'anymatch',
  'comnamebeg', 'comname', 'comnameend',
  'itistermscomname', 'itistermssciname', or
  'tsnsvernacular', 'tsnfullhir', 'tsnhirdown' .}
}
\value{
  A taxonomic serial number (TSN), and scientific or common
  name, depending on the searchtype parameter value used.
  See functions in \code{ritis}.
}
\description{
  \code{get_tsn} uses a variety of functions from the
  \code{ritis} package:
  \link{https://github.com/ropensci/ritis}.
}
\examples{
\dontrun{
get_tsn("Quercus douglasii", "sciname")
get_tsn(searchterm="Chironomus riparius", searchtype="sciname")
get_tsn(searchterm="polar bear", searchtype="comname")
lapply(c("Chironomus riparius","Quercus douglasii"), get_tsn, searchtype="sciname")
}
}

