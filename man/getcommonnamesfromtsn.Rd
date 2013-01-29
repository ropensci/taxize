\name{getcommonnamesfromtsn}
\alias{getcommonnamesfromtsn}
\title{Get common names from tsn}
\usage{
  getcommonnamesfromtsn(tsn = NA, ...,
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
  Get common names from tsn
}
\examples{
\dontrun{
getcommonnamesfromtsn(183833)
}
}

