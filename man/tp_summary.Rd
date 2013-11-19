\name{tp_summary}
\alias{tp_summary}
\title{Return summary data a taxon name with a given id.}
\usage{
  tp_summary(id, format = "json", output = "df",
    key = NULL, verbose = TRUE)
}
\arguments{
  \item{id}{the taxon identifier code}

  \item{format}{return in json or xml format (defaults to
  json)}

  \item{output}{raw = json or xml; or df = data.frame}

  \item{key}{Your Tropicos API key; loads from .Rprofile.}

  \item{verbose}{Print messages (default) or not, logical}
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

