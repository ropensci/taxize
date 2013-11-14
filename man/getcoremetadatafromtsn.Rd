\name{getcoremetadatafromtsn}
\alias{getcoremetadatafromtsn}
\title{Get core metadata from tsn}
\usage{
  getcoremetadatafromtsn(tsn = NA, ...,
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
  Get core metadata from tsn
}
\examples{
\dontrun{
getcoremetadatafromtsn(tsn = 28727)  # coverage and currrency data
getcoremetadatafromtsn(tsn = 183671)  # no coverage or currrency data
}
}
\keyword{internal}

