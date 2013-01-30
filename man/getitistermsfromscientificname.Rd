\name{getitistermsfromscientificname}
\alias{getitistermsfromscientificname}
\title{Get itis terms from scientific names}
\usage{
  getitistermsfromscientificname(srchkey = NA, ...,
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
\description{
  Get itis terms from scientific names
}
\examples{
\dontrun{
getitistermsfromscientificname(srchkey = "ursidae")
}
}

