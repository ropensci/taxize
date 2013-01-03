\name{col_downstream}
\alias{col_downstream}
\title{Use Catalogue of Life to get downstream taxa to a given taxonomic level.}
\usage{
  col_downstream(name = NULL, downto, format = NULL,
    start = NULL, checklist = NULL,
    url = "http://www.catalogueoflife.org/col/webservice")
}
\arguments{
  \item{name}{The string to search for. Only exact matches
  found the name given will be returned, unless one or
  wildcards are included in the search string.  An *
  (asterisk) character denotes a wildcard; a % (percentage)
  character may also be used. The name must be at least 3
  characters long, not counting wildcard characters.}

  \item{downto}{The taxonomic level you want to go down to.
  See examples below.  The taxonomic level IS case
  sensitive, and you do have to spell it correctly. See
  \code{data(rank_ref)} for spelling.}

  \item{checklist}{The year of the checklist to query, if
  you want a specific year's checklist instead of the
  lastest as default (numeric).}

  \item{url}{The base COL url for the function (should be
  left to default).}
}
\value{
  A list of data.frame's.
}
\description{
  Use Catalogue of Life to get downstream taxa to a given
  taxonomic level.
}
\details{
  Provide only names instead of id's
}
\examples{
\dontrun{
# Some basic examples
col_downstream(name="Apis", downto="Species")
col_downstream(name="Bryophyta", downto="Family")

# An example that takes a bit longer
col_downstream(name=c("Plantae","Animalia"), downto="Class")

# Using a checklist from a specific year
col_downstream(name="Bryophyta", downto="Family", checklist="2009")
}
}

