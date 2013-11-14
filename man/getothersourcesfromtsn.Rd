\name{getothersourcesfromtsn}
\alias{getothersourcesfromtsn}
\title{Returns a list of the other sources used for the TSN.}
\usage{
  getothersourcesfromtsn(tsn = NA, ...,
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
  Returns a list of the other sources used for the TSN.
}
\examples{
\dontrun{
getothersourcesfromtsn(tsn = 182662)
}
}
\keyword{internal}

