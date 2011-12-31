\name{get_tsn}
\alias{get_tsn}
\title{Get the TSN code for a search term.}
\usage{
  get_tsn(searchterm, searchtype, by_,
    curl = getCurlHandle())
}
\arguments{
  \item{searchterm}{Any common or scientific name.}

  \item{searchtype}{One of 'sciname', 'anymatch',
  'comnamebeg', 'comname', 'comnameend', 'terms',
  'itistermscomname', 'itistermssciname', or
  'tsnsvernacular', 'tsnfullhir', 'tsnhirdown' .}

  \item{by_}{One of 'name' (any common or scientific name)
  or 'tsn' (taxonomic serial number).}

  \item{curl}{If using in a loop, call getCurlHandle()
  first and pass the returned value in here (avoids
  unnecessary footprint)}
}
\value{
  A taxonomic serial number (TSN).
}
\description{
  Get the TSN code for a search term.
}
\examples{
\dontrun{
get_tsn("Quercus_douglasii", "sciname", by_="name")
}
}

