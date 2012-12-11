\name{col_children}
\alias{col_children}
\title{Search Catalogue of Life for taxonomic classifications.}
\usage{
  col_children(name = NULL, id = NULL, format = NULL,
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

  \item{id}{The record ID of the specific record to return
  (only for scientific names of species or infraspecific
  taxa)}

  \item{format}{format of the results returned. Valid
  values are format=xml and format=php; if the format
  parameter is omitted, the results are returned in the
  default XML format. If format=php then results are
  returned as a PHP array in serialized string format,
  which can be converted back to an array in PHP using the
  unserialize command}

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

  \item{url}{The base COL url for the function (should be
  left to default).}
}
\value{
  A list of data.frame's.
}
\description{
  Search Catalogue of Life for taxonomic classifications.
}
\details{
  You must provide one of name or id. The other parameters
  (format and start) are optional.
}
\examples{
\dontrun{
# A basic example
col_children(name="Apis")

# An example where there is no classification
col_children(id=11935941)

# Use a specific year's checklist
col_children(name="Apis", checklist="2012")
col_children(name="Apis", checklist="2009")

# Pass in many names or many id's
out <- col_children(name=c("Buteo","Apis","Accipiter","asdf"), checklist="2012")
out$Apis # get just the output you want
ldply(out) # or combine to one data.frame

# or pass many id's
out <- col_children(id=c(2346405,2344165,2346405), checklist="2012")
ldply(out) # combine to one data.frame
}
}

