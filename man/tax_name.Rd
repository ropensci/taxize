\name{tax_name}
\alias{tax_name}
\title{Get taxonomic names for a given taxonomic name query.}
\usage{
  tax_name(query = NULL, get = NULL, db = "itis",
    verbose = TRUE)
}
\arguments{
  \item{query}{Taxonomic name (character).}

  \item{get}{The rank of the taxonomic name to get
  (character).}

  \item{db}{The database to search from (character).}

  \item{verbose}{logical; If TRUE the actual taxon queried
  is printed on the console.}
}
\value{
  Taxonomic name for the searched taxon. If the taxon is
  not found NA is returned.
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
}
}

