\name{searchbycommonname}
\alias{searchbycommonname}
\title{Search for tsn by common name}
\usage{
searchbycommonname(srchkey = NA, ..., curl = getCurlHandle())
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
Search for tsn by common name
}
\examples{
\dontrun{
searchbycommonname("american bullfrog")
searchbycommonname("ferret-badger")
searchbycommonname(srchkey="polar bear")
}
}
\keyword{internal}

