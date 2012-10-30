\name{tp_namereferences}
\alias{tp_namereferences}
\title{Return all reference records for for a taxon name with a given id.}
\usage{
  tp_namereferences(id, format = "json", output = "df",
    url = "http://services.tropicos.org/Name/", key = NULL)
}
\arguments{
  \item{id}{the taxon identifier code}

  \item{format}{return in json or xml format (defaults to
  json)}

  \item{output}{raw = json or xml; or df = data.frame}

  \item{url}{The Tropicos url for the function (should be
  left to default).}

  \item{key}{Your Tropicos API key; loads from .Rprofile.}
}
\value{
  List or dataframe.
}
\description{
  Return all reference records for for a taxon name with a
  given id.
}
\examples{
\dontrun{
# Raw json or xml
tp_namereferences(id = 25509881, output = 'raw')

# Output as data.frame
tp_namereferences(id = 25509881)
}
}

