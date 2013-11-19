\name{getparenttsnfromtsn}
\alias{getparenttsnfromtsn}
\title{Returns the parent TSN for the entered TSN.}
\usage{
  getparenttsnfromtsn(tsn = NA, ...,
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
  Returns the parent TSN for the entered TSN.
}
\examples{
\dontrun{
getparenttsnfromtsn(tsn = 202385)
}
}
\keyword{internal}

