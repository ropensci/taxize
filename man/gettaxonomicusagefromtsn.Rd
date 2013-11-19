\name{gettaxonomicusagefromtsn}
\alias{gettaxonomicusagefromtsn}
\title{Returns the usage information for the TSN.}
\usage{
  gettaxonomicusagefromtsn(tsn = NA, ...,
    curl = getCurlHandle(), verbose = TRUE)
}
\arguments{
  \item{tsn}{TSN for a taxonomic group (numeric)}

  \item{...}{optional additional curl options (debugging
  tools mostly)}

  \item{curl}{If using in a loop, call getCurlHandle()
  first and pass the returned value in here (avoids
  unnecessary footprint)}

  \item{verbose}{Verbosity or not (default TRUE)}
}
\description{
  Returns the usage information for the TSN.
}
\examples{
\dontrun{
gettaxonomicusagefromtsn(tsn = 526852)
}
}
\keyword{internal}

