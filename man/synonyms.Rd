\name{synonyms}
\alias{synonyms}
\alias{synonyms.default}
\alias{synonyms.tpsid}
\alias{synonyms.tsn}
\title{Retrieve synonyms from various sources given input taxonomic names or identifiers.}
\usage{
  synonyms(...)

  \method{synonyms}{default} (x, db = NULL, ...)

  \method{synonyms}{tsn} (id, ...)

  \method{synonyms}{tpsid} (id, ...)
}
\arguments{
  \item{x}{character; taxons to query.}

  \item{db}{character; database to query. either
  \code{ncbi} or \code{itis}.}

  \item{id}{character; identifiers, returned by
  \code{\link[taxize]{get_tsn}} or
  \code{\link[taxize]{get_uid}}}

  \item{...}{Other passed arguments.}
}
\value{
  A named list of data.frames with the synonyms of every
  supplied taxa.
}
\description{
  Retrieve synonyms from various sources given input
  taxonomic names or identifiers.
}
\note{
  If IDs are supplied directly (not from the \code{get_*}
  functions) you must specify the type of ID.
}
\examples{
\dontrun{
# Plug in taxon names directly
synonyms("Poa annua", db="itis")
synonyms(c("Poa annua",'Pinus contorta','Puma concolor'), db="itis")
synonyms("Poa annua", db="tropicos")
synonyms("Pinus contorta", db="tropicos")
synonyms(c("Poa annua",'Pinus contorta'), db="tropicos")

# Use methods for get_uid and get_tsn
synonyms(get_tsn("Poa annua"))
}
}
\seealso{
  \code{\link[taxize]{get_tsn}},
  \code{\link[taxize]{get_uid}}, ,
  \code{\link[taxize]{get_tpsid}}
}

