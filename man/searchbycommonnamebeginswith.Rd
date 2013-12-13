\name{searchbycommonnamebeginswith}
\alias{searchbycommonnamebeginswith}
\title{Search for tsn by common name beginning with}
\usage{
searchbycommonnamebeginswith(srchkey = NA, ..., curl = getCurlHandle())
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
Search for tsn by common name beginning with
}
\examples{
\dontrun{
searchbycommonnamebeginswith("inch")
}
}
\keyword{internal}

