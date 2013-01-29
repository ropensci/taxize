\name{getanymatchcount}
\alias{getanymatchcount}
\title{Get any match count.}
\usage{
  getanymatchcount(srchkey = NA, ...,
    curl = getCurlHandle())
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
\value{
  An integer containing the number of matches the search
  will return.
}
\description{
  Get any match count.
}
\examples{
\dontrun{
getanymatchcount(202385)
getanymatchcount("dolphin")
}
}

