\name{classification}
\alias{cbind.classification}
\alias{cbind.classification_ids}
\alias{classification}
\alias{classification.colid}
\alias{classification.default}
\alias{classification.eolid}
\alias{classification.ids}
\alias{classification.tpsid}
\alias{classification.tsn}
\alias{classification.uid}
\alias{rbind.classification}
\alias{rbind.classification_ids}
\title{Retrieve the taxonomic hierarchy for a given taxon ID.}
\usage{
classification(...)

\method{classification}{default}(x, db = NULL, ...)

\method{classification}{tsn}(id, ...)

\method{classification}{uid}(id, ...)

\method{classification}{eolid}(id, key = NULL, callopts = list(), ...)

\method{classification}{colid}(id, start = NULL, checklist = NULL, ...)

\method{classification}{tpsid}(id, key = NULL, callopts = list(), ...)

\method{classification}{ids}(id, ...)

\method{cbind}{classification}(...)

\method{rbind}{classification}(...)

\method{cbind}{classification_ids}(...)

\method{rbind}{classification_ids}(...)
}
\arguments{
  \item{x}{character; taxons to query.}

  \item{db}{character; database to query. either
  \code{ncbi}, \code{itis}, \code{eol}, \code{col} or
  \code{tropicos}.}

  \item{id}{character; identifiers, returned by
  \code{\link[taxize]{get_tsn}},
  \code{\link[taxize]{get_uid}},
  \code{\link[taxize]{get_eolid}},
  \code{\link[taxize]{get_colid}}, or
  \code{\link[taxize]{get_tpsid}}.}

  \item{...}{Other arguments passed to
  \code{\link[taxize]{get_tsn}},
  \code{\link[taxize]{get_uid}},
  \code{\link[taxize]{get_eolid}},
  \code{\link[taxize]{get_colid}}, or
  \code{\link[taxize]{get_tpsid}}.}

  \item{start}{The first record to return. If omitted, the
  results are returned from the first record (start=0).
  This is useful if the total number of results is larger
  than the maximum number of results returned by a single
  Web service query (currently the maximum number of
  results returned by a single query is 500 for terse
  queries and 50 for full queries).}

  \item{checklist}{character; The year of the checklist to
  query, if you want a specific year's checklist instead of
  the lastest as default (numeric).}

  \item{key}{Your API key; loads from .Rprofile.}

  \item{callopts}{Further args passed on to httr::GET.}
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

# Pass many ids from class "ids"
out <- get_ids(names="Poa annua", db = c('ncbi','itis','col','eol','tropicos'))
cl <- classification(out)

# Bind length-wise
cbind(cl)

# Bind width-wise
rbind(cl)
}
}
\seealso{
\code{\link[taxize]{get_tsn}},
\code{\link[taxize]{get_uid}},
\code{\link[taxize]{get_eolid}},
\code{\link[taxize]{get_colid}},
\code{\link[taxize]{get_tpsid}}
}

