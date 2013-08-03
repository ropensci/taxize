\name{getacceptednamesfromtsn}
\alias{getacceptednamesfromtsn}
\title{Get accepted names from tsn}
\usage{
  getacceptednamesfromtsn(tsn = NA, ...,
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
  Get accepted names from tsn
}
\examples{
\dontrun{
# TSN accepted - good name
getacceptednamesfromtsn('208527')

# TSN not accepted - input TSN is old name
getacceptednamesfromtsn(tsn='504239')

# TSN not accepted - input TSN is old name
getacceptednamesfromtsn('504239', FALSE)
}
}

