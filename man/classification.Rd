\name{classification}
\alias{classification}
\alias{classification.tsn}
\alias{classification.uid}
\title{Get taxonomic hierarchy for a given taxon ID.}
\usage{
  classification(x, ID = NULL, locally = FALSE, ...)

  \method{classification}{tsn} (x, ...)

  \method{classification}{uid} (x, ...)
}
\arguments{
  \item{x}{IDs from \code{get_tsn()} or \code{get_uid()}.}

  \item{ID}{type of identifier, either 'uid' or 'tsn'}

  \item{...}{Currently not used}

  \item{locally}{If TRUE, queries are run locally in
  sqlite3; if FALSE (the default), queries are run against
  the ITIS web API. locally=TRUE should be faster in almost
  all cases.}
}
\value{
  Classification of taxons in a list of data.frames.
}
\description{
  Get taxonomic hierarchy for a given taxon ID.
}
\note{
  If IDs are supplied directly (not from the get_*
  functions) use the methods classification.ncbi() or
  classification.tsn() directly. See examples.
}
\examples{
\dontrun{
classification(get_uid(c("Chironomus riparius", "aaa vva")))
classification(get_tsn(c("Chironomus riparius", "aaa vva"), "sciname"))

# must specify Identifier, when not used with get_*()
classification(315576, ID = "uid")
classification(180544, "tsn")
}
}

