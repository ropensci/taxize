\name{tp_namereferences}
\alias{tp_namereferences}
\title{Return all reference records for for a taxon name with a given id.}
\usage{
tp_namereferences(id, format = "json", output = "df", key = NULL,
  verbose = TRUE)
}
\arguments{
  \item{id}{the taxon identifier code}

  \item{format}{return in json or xml format (defaults to
  json)}

  \item{output}{raw = json or xml; or df = data.frame}

  \item{key}{Your Tropicos API key; loads from .Rprofile.}

  \item{verbose}{Print messages (default) or not, logical}
}
\description{
Function name changed to tp_refs.
}

