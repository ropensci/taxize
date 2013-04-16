\name{tax_name}
\alias{tax_name}
\title{Get taxonomic names for a given taxonomic name query.}
\usage{
  tax_name(query = NULL, get = NULL, db = "itis",
    pref = "ncbi", verbose = TRUE, ...)
}
\arguments{
  \item{query}{character; Vector of taxonomic names to
  query.}

  \item{get}{character; The ranks of the taxonomic name to
  get.}

  \item{db}{character; The database to search from: 'tis',
  'ncbi' or 'both'.  If 'both' both NCBI and ITIS will be
  queried. Result will be the union of both.}

  \item{pref}{If db='both', sets the preference for the
  union. Either 'ncbi' or 'itis'.}

  \item{verbose}{logical; If TRUE the actual taxon queried
  is printed on the console.}

  \item{locally}{If TRUE, queries are run locally in
  sqlite3; if FALSE (the default), queries are run against
  the ITIS web API. locally=TRUE should be faster in almost
  all cases.}
}
\value{
  A data.frame with one column for every queried rank.
}
\description{
  Specify what database you want to use: itis or ncbi.
}
\examples{
\dontrun{
# A case where itis and ncbi use the same names
tax_name(query="Helianthus annuus", get="family", db="itis")
tax_name(query="Helianthus annuus", get="family", db="ncbi")

# Case where itis and ncbi use different names
tax_name(query="Helianthus annuus", get="kingdom", db="itis")
tax_name(query="Helianthus annuus", get="kingdom", db="ncbi")

# multiple get arguments
tax_name(query=c("Helianthus annuus","Baetis rhodani"), get=c("genus", "kingdom"), db="ncbi")
tax_name(query="xxx", get=c("genus", "kingdom"), db="itis")

# query both
tax_name(query=c("Helianthus annuus", 'Baetis rhodani'), get=c("genus", "kingdom"), db="both")
}
}

