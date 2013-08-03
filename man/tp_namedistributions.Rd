\name{tp_namedistributions}
\alias{tp_namedistributions}
\title{Return all distribution records for for a taxon name with a given id.}
\usage{
  tp_namedistributions(id, format = "json", output = "df",
    key = NULL)
}
\arguments{
  \item{id}{the taxon identifier code}

  \item{format}{return in json or xml format (defaults to
  json)}

  \item{output}{raw = json or xml; or df = data.frame}

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
tp_namedistributions(id = 25509881, output = 'raw')

# Output as data.frame
df <- tp_namedistributions(id = 25509881)
df[df$category \%in\% 'Location',] # just location data
df[df$category \%in\% 'Reference',] # just reference data
}
}

