\name{get_ids}
\alias{get_ids}
\title{Retrieve taxonomic identifiers for a given taxon name.}
\usage{
get_ids(names, db = NULL, ...)
}
\arguments{
  \item{names}{character; Taxonomic name to query.}

  \item{db}{character; database to query. One or more of
  \code{ncbi}, \code{itis}, \code{eol}, \code{col}, and/or
  \code{tropicos}}

  \item{...}{Other arguments passed to
  \code{\link[taxize]{get_tsn}},
  \code{\link[taxize]{get_uid}},
  \code{\link[taxize]{get_eolid}},
  \code{\link[taxize]{get_colid}}, or
  \code{\link[taxize]{get_tpsid}}.}
}
\value{
A vector of taxonomic identifiers, each retaining their
respective S3 classes so that each element can be passed on
to another function (see e.g.'s).
}
\description{
This is a convenience function to get identifiers across
all data sources. You can use other \code{get_*} functions
to get identifiers from specific sources if you like.
}
\note{
There is a timeout of 1/3 seconds between queries to NCBI.
}
\examples{
\dontrun{
# Plug in taxon names directly
get_ids(names="Chironomus riparius", db = 'ncbi')
get_ids(names=c("Chironomus riparius", "Poa annua"), db = 'ncbi')
get_ids(names=c("Chironomus riparius", "Poa annua"), db = c('ncbi','itis'))
get_ids(names=c("Chironomus riparius", "Poa annua"), db = c('ncbi','itis','col'))
get_ids(names="Poa annua", db = c('ncbi','itis','col','eol','tropicos'))
get_ids(names="ava avvva", db = c('ncbi','itis','col','eol','tropicos'))
get_ids(names="ava avvva", db = c('ncbi','itis','col','eol','tropicos'), verbose=FALSE)

# Pass on to other functions
out <- get_ids(names="Poa annua", db = c('ncbi','itis','col','eol','tropicos'))
classification(out$itis)
synonyms(out$tropicos)
}
}
\seealso{
\code{\link[taxize]{get_tsn}},
\code{\link[taxize]{get_uid}},
\code{\link[taxize]{get_eolid}},
\code{\link[taxize]{get_colid}},
\code{\link[taxize]{get_tpsid}}
}

