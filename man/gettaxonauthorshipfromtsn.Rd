\name{gettaxonauthorshipfromtsn}
\alias{gettaxonauthorshipfromtsn}
\title{Returns the author information for the TSN.}
\usage{
  gettaxonauthorshipfromtsn(tsn = NA, ...,
    curl = getCurlHandle())
}
\arguments{
  \item{tsn}{TSN for a taxonomic group (numeric)}

  \item{...}{optional additional curl options (debugging
  tools mostly)}

  \item{curl}{If using in a loop, call getCurlHandle()
  first and pass the returned value in here (avoids
  unnecessary footprint)}
}
\description{
  Returns the author information for the TSN.
}
\examples{
\dontrun{
gettaxonauthorshipfromtsn(tsn = 183671)
}
}

