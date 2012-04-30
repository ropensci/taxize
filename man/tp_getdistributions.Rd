\name{tp_getdistributions}
\alias{tp_getdistributions}
\title{Return all distribution records for for a taxon name with a given id.}
\usage{
  tp_getdistributions(id, format = "json", output = "df",
    url = "http://services.tropicos.org/Name/",
    key = getOption("tropicoskey", stop("need an API key for Tropicos")))
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
  Return all distribution records for for a taxon name with
  a given id.
}
\examples{
\dontrun{
# Raw json or xml
tp_getdistributions(id = 25509881, output = 'raw')

# Output as data.frame
df <- tp_getdistributions(id = 25509881)
df[df$category \%in\% 'Location',] # just location data
df[df$category \%in\% 'Reference',] # just reference data
}
}

