\name{gethierarchydownfromtsn}
\alias{gethierarchydownfromtsn}
\title{Get hierarchy down from tsn}
\usage{
gethierarchydownfromtsn(tsn = NA, ..., curl = getCurlHandle(),
  verbose = TRUE)
}
\arguments{
  \item{verbose}{Verbosity or not (default TRUE)}

  \item{tsn}{TSN for a taxonomic group (numeric)}

  \item{...}{optional additional curl options (debugging
  tools mostly)}

  \item{curl}{If using in a loop, call getCurlHandle()
  first and pass the returned value in here (avoids
  unnecessary footprint)}
}
\description{
Get hierarchy down from tsn
}
\examples{
\dontrun{
gethierarchydownfromtsn(tsn = 161030)
}
}
\keyword{internal}

