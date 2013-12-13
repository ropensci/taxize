\name{apg_lookup}
\alias{apg_lookup}
\title{Lookup in the APGII taxonomy and replace family names.}
\usage{
apg_lookup(taxa, rank = "family")
}
\arguments{
  \item{taxa}{Taxonomic name to lookup a synonym for in
  APGII taxonomy.}

  \item{rank}{Taxonomic rank to lookup a synonym for. One
  of family or order.}
}
\value{
A APGII family or order name, or the original name if no
match.
}
\description{
Lookup in the APGII taxonomy and replace family names.
}
\examples{
\dontrun{
# New name found in the APGII taxonomy
apg_lookup(taxa = "Hyacinthaceae", rank = "family")

# Name not found in the APGII taxonomy
apg_lookup(taxa = "Asteraceae", rank = "family")
}
}

