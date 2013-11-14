\name{getjurisdictionaloriginfromtsn}
\alias{getjurisdictionaloriginfromtsn}
\title{Get jurisdictional origin from tsn}
\usage{
  getjurisdictionaloriginfromtsn(tsn = NA, ...,
    curl = getCurlHandle(), verbose = TRUE)
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
  Get jurisdictional origin from tsn
}
\examples{
\dontrun{
getjurisdictionaloriginfromtsn(tsn = 2180543)
}
}
\keyword{internal}

