\name{col_search}
\alias{col_search}
\title{Search Catalogue of Life for taxonomic IDs}
\usage{
  col_search(name = NULL, id = NULL, start = NULL,
    checklist = NULL)
}
\arguments{
  \item{name}{The string to search for. Only exact matches
  found the name given will be returned, unless one or
  wildcards are included in the search string. An *
  (asterisk) character denotes a wildcard; a % (percentage)
  character may also be used. The name must be at least 3
  characters long, not counting wildcard characters.}

  \item{id}{The record ID of the specific record to return
  (only for scientific names of species or infraspecific
  taxa)}

  \item{start}{The first record to return. If omitted, the
  results are returned from the first record (start=0).
  This is useful if the total number of results is larger
  than the maximum number of results returned by a single
  Web service query (currently the maximum number of
  results returned by a single query is 500 for terse
  queries and 50 for full queries).}

  \item{checklist}{The year of the checklist to query, if
  you want a specific year's checklist instead of the
  lastest as default (numeric).}
}
\value{
  A list of data.frame's.
}
\description{
  Search Catalogue of Life for taxonomic IDs
}
\details{
  You must provide one of name or id. The other parameters
  (format and start) are optional.
}
\examples{
\dontrun{
# A basic example
col_search(name="Apis")

# Many names
col_search(name=c("Apis","Puma concolor"))

# An example where there is no data
col_search(id=11935941)
}
}

