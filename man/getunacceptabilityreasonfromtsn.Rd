\name{getunacceptabilityreasonfromtsn}
\alias{getunacceptabilityreasonfromtsn}
\title{Returns the unacceptability reason, if any, for the TSN.}
\usage{
getunacceptabilityreasonfromtsn(tsn = NA, ..., curl = getCurlHandle())
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
Returns the unacceptability reason, if any, for the TSN.
}
\examples{
\dontrun{
getunacceptabilityreasonfromtsn(tsn = 183671)
}
}
\keyword{internal}

