\name{classification}
\alias{classification}
\alias{classification.tsn}
\alias{classification.uid}
\title{Retrieve the taxonomic hierarchy for a given taxon ID.}
\usage{
  classification(x, ID = NULL, ...)

  \method{classification}{tsn} (x, ...)

  \method{classification}{uid} (x)
}
\arguments{
  \item{x}{character; IDs as returned by
  \code{\link[taxize]{get_tsn}} or
  \code{\link[taxize]{get_uid}}.}

  \item{ID}{character, type of identifier, either 'uid' or
  'tsn'.}

  \item{...}{Other arguments passed to
  \code{\link[taxize]{getfullhierarchyfromtsn}}.}
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
classification(get_uid(c("Chironomus riparius", "aaa vva")))
classification(get_tsn(c("Chironomus riparius", "aaa vva"), "sciname"))

# must specify Identifier, when not used with get_*()
classification(315576, ID = "uid")
classification(180544, "tsn")
}
}
\seealso{
  \code{\link[taxize]{get_tsn}},
  \code{\link[taxize]{get_uid}}
}

