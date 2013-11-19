\name{getreviewyearfromtsn}
\alias{getreviewyearfromtsn}
\title{Returns the review year for the TSN.}
\usage{
  getreviewyearfromtsn(tsn = NA, ...,
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
  Returns the review year for the TSN.
}
\examples{
\dontrun{
getreviewyearfromtsn(tsn = 180541)
}
}
\keyword{internal}

