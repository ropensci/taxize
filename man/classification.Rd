\name{classification}
\alias{classification}
\alias{classification.ncbi}
\alias{classification.tsn}
\title{Get taxonomic hierarchy for a given taxon ID.}
\usage{
  classification(x, ...)

  \method{classification}{tsn} (x)

  \method{classification}{ncbi} (x)
}
\arguments{
  \item{x}{IDs from \code{get_tsn()} or \code{get_uid()}.}
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
# Fails
classification(1111)
# Use appropiate method instead
taxize:::classification.ncbi(315576)
}
}

