\name{getpublicationsfromtsn}
\alias{getpublicationsfromtsn}
\title{Returns a list of the pulications used for the TSN.}
\usage{
  getpublicationsfromtsn(tsn = NA, ...,
    curl = getCurlHandle(), verbose = TRUE)
}
\arguments{
  \item{verbose}{Verbosity or not (default TRUE)}

  \item{tsn}{TSN for a taxonomic group (numeric)}

  \item{...}{optional additional curl options (debugging
  tools mostly)}

  \item{curl}{If using in a loop, call getCurlHandle()
  first and pass the returned value in here (avoids
  unnecessary footprint)}
}
\description{
  Returns a list of the pulications used for the TSN.
}
\examples{
\dontrun{
getpublicationsfromtsn(tsn = 70340)
}
}
\keyword{internal}

