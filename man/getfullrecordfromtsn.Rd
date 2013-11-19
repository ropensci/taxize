\name{getfullrecordfromtsn}
\alias{getfullrecordfromtsn}
\title{Get full record from TSN.}
\usage{
  getfullrecordfromtsn(tsn = NA, ...,
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
  Get full record from TSN.
}
\examples{
\dontrun{
getfullrecordfromtsn(tsn = 504239)
getfullrecordfromtsn(tsn = 202385)
getfullrecordfromtsn(tsn = 183833)
getfullrecordfromtsn(tsn = 183833, verbose=FALSE)
}
}
\keyword{internal}

