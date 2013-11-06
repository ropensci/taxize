\name{tp_synonyms}
\alias{tp_synonyms}
\title{Return all synonyms for a taxon name with a given id.}
\usage{
  tp_synonyms(id, format = "json", key = NULL)
}
\arguments{
  \item{id}{the taxon identifier code}

  \item{format}{return in json or xml format (defaults to
  json)}

  \item{key}{Your Tropicos API key; loads from .Rprofile.}
}
\value{
  List or dataframe.
}
\description{
  Return all synonyms for a taxon name with a given id.
}
\examples{
\dontrun{
tp_synonyms(id = 25509881)
tp_synonyms(id = 25509881, output = 'raw')
}
}

