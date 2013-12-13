\name{searchbycommonnameendswith}
\alias{searchbycommonnameendswith}
\title{Search for tsn by common name ending with}
\usage{
searchbycommonnameendswith(srchkey = NA, ..., curl = getCurlHandle())
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
Search for tsn by common name ending with
}
\examples{
\dontrun{
searchbycommonnameendswith(srchkey="snake")
}
}
\keyword{internal}

