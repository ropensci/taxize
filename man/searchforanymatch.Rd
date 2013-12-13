\name{searchforanymatch}
\alias{searchforanymatch}
\title{Search for any match}
\usage{
searchforanymatch(srchkey = NA, ..., curl = getCurlHandle())
}
\arguments{
  \item{srchkey}{text or taxonomic serial number (TSN)
  (character or numeric)}

  \item{...}{optional additional curl options (debugging
  tools mostly)}

  \item{curl}{If using in a loop, call getCurlHandle()
  first and pass the returned value in here (avoids
  unnecessary footprint)}
}
\description{
Search for any match
}
\examples{
\dontrun{
searchforanymatch(srchkey = 202385)
searchforanymatch(srchkey = "dolphin")
}
}
\keyword{internal}

