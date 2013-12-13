\name{tp_dist}
\alias{tp_dist}
\title{Return all distribution records for for a taxon name with a given id.}
\usage{
tp_dist(id, key = NULL, callopts = list())
}
\arguments{
  \item{id}{the taxon identifier code}

  \item{key}{Your Tropicos API key; loads from .Rprofile.
  Or you can passin your key in this arg.}

  \item{callopts}{Further args passed on to httr::GET}
}
\value{
List of two data.frame's, one named "location", and one
"reference".
}
\description{
Return all distribution records for for a taxon name with a
given id.
}
\examples{
\dontrun{
# Query using a taxon name Id
out <- tp_dist(id = 25509881)
## just location data
head(out[['location']])
## just reference data
head(out[['reference']])
}
}
\references{
\url{http://services.tropicos.org/help?method=GetNameDistributionsXml}
}

