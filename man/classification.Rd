\name{classification}
\alias{classification}
\alias{classification.tsn}
\alias{classification.uid}
\title{Retrieve the taxonomic hierarchy for a given taxon ID.}
\usage{
  classification(x, db = NULL, ...)

  \method{classification}{tsn} (x, ...)

  \method{classification}{uid} (id, ...)
}
\arguments{
  \item{x}{character; taxons to query.}

  \item{db}{character; database to query. either
  \code{ncbi} or \code{itis}.}

  \item{id}{character; identifiers, as returned by
  \code{\link[taxize]{get_tsn}} or
  \code{\link[taxize]{get_uid}}}

  \item{...}{Other passed arguments.}
}
\value{
  A list of data.frames with the taxonomic classifcation of
  every supplied taxa.
}
\description{
  Retrieve the taxonomic hierarchy for a given taxon ID.
}
\note{
  If IDs are supplied directly (not from the \code{get_*}
  functions) you must specify the type of ID. There is a
  timeout of 1/3 seconds between querries to NCBI.
}
\examples{
\dontrun{
# Plug in taxon names directly
classification(c("Chironomus riparius", "aaa vva"), db = 'ncbi')
classification(c("Chironomus riparius", "aaa vva"), db = 'itis')

# Use methods for get_uid and get_tsn
classification(get_uid(c("Chironomus riparius", "aaa vva")))
classification(get_tsn(c("Chironomus riparius", "aaa vva")))
}
}
\seealso{
  \code{\link[taxize]{get_tsn}},
  \code{\link[taxize]{get_uid}}
}

