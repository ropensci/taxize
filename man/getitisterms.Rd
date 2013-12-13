\name{getitisterms}
\alias{getitisterms}
\title{Get itis terms from common names}
\usage{
getitisterms(srchkey = NA, ..., curl = getCurlHandle())
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
Get itis terms from common names
}
\examples{
\dontrun{
getitisterms(srchkey="bear")
}
}
\keyword{internal}

