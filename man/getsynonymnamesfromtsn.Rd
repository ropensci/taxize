\name{getsynonymnamesfromtsn}
\alias{getsynonymnamesfromtsn}
\title{Returns a list of the synonyms (if any) for the TSN.}
\usage{
  getsynonymnamesfromtsn(tsn = NA, ...,
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
  Returns a list of the synonyms (if any) for the TSN.
}
\examples{
\dontrun{
getsynonymnamesfromtsn(tsn = 183671) # tsn not accepted
getsynonymnamesfromtsn(tsn = 526852) # tsn accepted
}
}

