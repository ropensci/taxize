\name{classification}
\alias{classification}
\alias{classification.colid}
\alias{classification.default}
\alias{classification.eolid}
\alias{classification.tpsid}
\alias{classification.tsn}
\alias{classification.uid}
\title{Retrieve the taxonomic hierarchy for a given taxon ID.}
\usage{
  classification(...)

  \method{classification}{default} (x, db = NULL, ...)

  \method{classification}{tsn} (id, ...)

  \method{classification}{uid} (id, ...)

  \method{classification}{eolid} (id, ...)

  \method{classification}{colid} (id, ...)

  \method{classification}{tpsid} (id, ...)
}
\arguments{
  \item{x}{character; taxons to query.}

  \item{db}{character; database to query. either
  \code{ncbi}, \code{itis}, \code{eol}, or \code{tropicos}}

  \item{id}{character; identifiers, returned by
  \code{\link[taxize]{get_tsn}},
  \code{\link[taxize]{get_uid}},
  \code{\link[taxize]{get_eolid}},
  \code{\link[taxize]{get_colid}}, or
  \code{\link[taxize]{get_tpsid}}}

  \item{...}{Other arguments passed to
  \code{\link[taxize]{get_tsn}},
  \code{\link[taxize]{get_uid}},
  \code{\link[taxize]{get_eolid}},
  \code{\link[taxize]{get_colid}}, or
  \code{\link[taxize]{get_tpsid}}.}
}
\value{
  A named list of data.frames with the taxonomic
  classifcation of every supplied taxa.
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
classification(c("Chironomus riparius", "aaa vva"), db = 'eol')
classification(c("Chironomus riparius", "aaa vva"), db = 'col')
classification(c("Poa annua", "aaa vva"), db = 'tropicos')

# Use methods for get_uid, get_tsn, get_eolid, get_colid, get_tpsid
classification(get_uid(c("Chironomus riparius", "Puma concolor")))

classification(get_uid(c("Chironomus riparius", "aaa vva")))
classification(get_tsn(c("Chironomus riparius", "aaa vva")))
classification(get_eolid(c("Chironomus riparius", "aaa vva")))
classification(get_colid(c("Chironomus riparius", "aaa vva")))
classification(get_tpsid(c("Poa annua", "aaa vva")))
}
}
\seealso{
  \code{\link[taxize]{get_tsn}},
  \code{\link[taxize]{get_uid}},
  \code{\link[taxize]{get_eolid}},
  \code{\link[taxize]{get_colid}},
  \code{\link[taxize]{get_tpsid}}
}

