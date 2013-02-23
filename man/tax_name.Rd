\name{tax_name}
\alias{tax_name}
\title{Get taxonomic names for a given taxonomic name query.}
\usage{
  tax_name(query = NULL, get = NULL, db = "itis",
    verbose = TRUE)
}
\arguments{
  \item{query}{character; Vector of taxonomic names to
  query.}

  \item{get}{character; The ranks of the taxonomic name to
  get.}

  \item{db}{character; The database to search from: 'tis'
  or 'ncbi'.}

  \item{verbose}{logical; If TRUE the actual taxon queried
  is printed on the console.}
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
tax_name(query="Helianthus annuus", get=c("genus", "kingdom"), db="ncbi")
tax_name(query="xxx", get=c("genus", "kingdom"), db="itis")
}
}

