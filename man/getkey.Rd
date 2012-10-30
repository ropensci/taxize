\name{getkey}
\alias{getkey}
\title{Function to get API key.}
\usage{
  getkey(x = NULL)
}
\arguments{
  \item{x}{An API key, defaults to NULL.}
}
\description{
  Checks first to get key from your .Rprofile file for an
  API key with the name 'tropicoskey'. If it is not found,
  the default key is used.
}
\examples{
\dontrun{
getkey()
}
}

