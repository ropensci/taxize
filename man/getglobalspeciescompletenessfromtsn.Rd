\name{getglobalspeciescompletenessfromtsn}
\alias{getglobalspeciescompletenessfromtsn}
\title{Get global species completeness from tsn}
\usage{
  getglobalspeciescompletenessfromtsn(tsn = NA, ...,
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
  Get global species completeness from tsn
}
\examples{
\dontrun{
getglobalspeciescompletenessfromtsn(tsn = 180541)
}
}
\keyword{internal}

