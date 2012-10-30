\name{tp_summary}
\alias{tp_summary}
\title{Return summary data a taxon name with a given id.}
\usage{
  tp_summary(id, format = "json", output = "df",
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
  Return summary data a taxon name with a given id.
}
\examples{
\dontrun{
tp_summary(id = 25509881)
tp_summary(id = 25509881, output = 'raw')
}
}

