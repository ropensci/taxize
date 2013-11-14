\name{searchbyscientificname}
\alias{searchbyscientificname}
\title{Search by scientific name}
\usage{
  searchbyscientificname(srchkey = NA, ...,
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
  Search by scientific name
}
\examples{
\dontrun{
searchbyscientificname("Tardigrada")
searchbyscientificname("Quercus douglasii")
}
}
\keyword{internal}

