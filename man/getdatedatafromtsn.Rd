\name{getdatedatafromtsn}
\alias{getdatedatafromtsn}
\title{Get date data from tsn}
\usage{
  getdatedatafromtsn(tsn = NA, ..., curl = getCurlHandle())
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
  Get date data from tsn
}
\examples{
\dontrun{
getdatedatafromtsn(tsn = 180543)
}
}

