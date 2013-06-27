\name{tax_rank}
\alias{tax_rank}
\title{Get rank for a given taxonomic name.}
\usage{
  tax_rank(query = NULL, db = "itis", pref = "ncbi",
    verbose = TRUE)
}
\arguments{
  \item{query}{character; Vector of taxonomic names to
  query.}

  \item{db}{character; The database to search from: 'tis',
  'ncbi' or 'both'.  If 'both' both NCBI and ITIS will be
  queried. Result will be the union of both.}

  \item{pref}{If db = 'both', sets the preference for the
  union. Either 'ncbi' or 'itis'.}

  \item{verbose}{logical; If TRUE the actual taxon queried
  is printed on the console.}
}
\value{
  A data.frame with one column for every queried taxon.
}
\description{
  Retrieve rank of a taxon.
}
\note{
  While \code{\link[taxize]{tax_name}} returns the name of
  a specified rank, \code{\link[taxize]{tax_rank}} returns
  the actual rank of the taxon.
}
\examples{
\dontrun{
tax_rank(query = "Helianthus annuus", db = "itis")
tax_rank(query = "Helianthus annuus", db = "ncbi")
tax_rank(query = "Helianthus, db = "itis")
tax_rank(query = "Baetis rhodani", db = "itis")

# query both
tax_rank(query=c("Helianthus annuus", 'Baetis rhodani'), db="both")

# An alternative way would be to use \\link{classification} and sapply over the list
x <- 'Baetis'
classi <- classification(get_uid(x))
sapply(classi, function(x) x[nrow(x), 'Rank'])
}
}
\seealso{
  \code{\link[taxize]{classification}},
  \code{\link[taxize]{tax_name}}
}

