\name{tp_accnames}
\alias{tp_accnames}
\title{Return all accepted names for a taxon name with a given id.}
\usage{
tp_accnames(id, format = "json", output = "df", key = NULL)
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
Return all accepted names for a taxon name with a given id.
}
\examples{
\dontrun{
tp_accnames(id = 25503923)
tp_accnames(id = 25503923, output = 'raw')
}
}

