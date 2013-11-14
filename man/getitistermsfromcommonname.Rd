\name{getitistermsfromcommonname}
\alias{getitistermsfromcommonname}
\title{Get itis terms from common names}
\usage{
  getitistermsfromcommonname(srchkey = NA, ...,
    curl = getCurlHandle())
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
getitistermsfromcommonname("buya")
}
}
\keyword{internal}

