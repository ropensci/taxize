\name{tp_getacceptednames}
\alias{tp_getacceptednames}
\title{Return all accepted names for a taxon name with a given id.}
\usage{
  tp_getacceptednames(id, format = "json", output = "df",
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
  Return all accepted names for a taxon name with a given
  id.
}
\examples{
\dontrun{
tp_getacceptednames(id = 25503923)
tp_getacceptednames(id = 25503923, output = 'raw')
}
}

