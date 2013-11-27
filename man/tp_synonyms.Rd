\name{tp_synonyms}
\alias{tp_synonyms}
\title{Return all synonyms for a taxon name with a given id.}
\usage{
  tp_synonyms(id, key = NULL, verbose = TRUE)
}
\arguments{
  \item{id}{the taxon identifier code}

  \item{key}{Your Tropicos API key; loads from .Rprofile.}

  \item{verbose}{Verbose or not}
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
}
}

