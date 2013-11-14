\name{getcommentdetailfromtsn}
\alias{getcommentdetailfromtsn}
\title{Get comment detail from TSN}
\usage{
  getcommentdetailfromtsn(tsn = NA, ...,
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
\value{
  A data.frame with results.
}
\description{
  Get comment detail from TSN
}
\examples{
\dontrun{
getcommentdetailfromtsn(180543)
}
}
\keyword{internal}

